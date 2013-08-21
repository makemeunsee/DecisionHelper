import scala.collection.immutable.HashMap
import scala.swing.Dialog
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Swing
import scala.swing.Label
import javax.swing.WindowConstants
import scala.swing.event.Key
import scala.swing.event.KeyReleased
import scala.Array.canBuildFrom
import scala.io.Codec.string2codec


object DecisionHelper {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0))("UTF-8").getLines()
    // read category weights from first line, build a weighted list
    val weightedCategories = lines.next.split(" / ").map({ el =>
      val w_cat = el.split(":")
      val (cat, weight) = (w_cat(0), Integer.valueOf(w_cat(1)))
      List.fill(weight)(cat)
    }).flatten
    // list activities by category, as given by all other lines 
    val byCategory = lines.foldLeft(Map.empty[String, List[String]])(updateCategoryMap)
    // pick random category, get associated activity list
    val rndCat = byCategory(randomElement(weightedCategories))
    // pick random activity in category
    val activity = randomElement(rndCat)
    // show chosen activity
    println(activity)
    new InfoDialog(activity)
  }
  
  def updateCategoryMap(currentMap: Map[String, List[String]], rawLine: String): Map[String, List[String]] = {
    // line syntax is: [activity]( / [category])*
    val pieces = rawLine.split(" / ")
    val activity = pieces(0)
    // update category map with each category listed along this activity 
    pieces.drop(1).foldLeft(currentMap)((oldMap, cat) => oldMap.updated(cat, activity :: oldMap.getOrElse(cat, Nil)))
  }
  
  def randomElement[T](seq: Seq[T]): T = seq((math.random * seq.length).toInt)
  
  // centered, always on top dialog showing the decision
  class InfoDialog(val activity: String) extends Dialog {
  
    title = "What do?"
    modal = true
    resizable = false
    
    contents = new BorderPanel {
      layout(new BoxPanel(Orientation.Vertical) {
        border = Swing.EmptyBorder(5,5,5,5)
        contents += new Label(activity)
      }) = scala.swing.BorderPanel.Position.Center
      
      listenTo(keys)
      reactions += { case KeyReleased(_, Key.Escape, _, _) => dispose() }
      reactions += { case KeyReleased(_, Key.Enter, _, _) => dispose() }
      
      focusable = true
      requestFocus()

    }
    
    peer.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    peer.setAlwaysOnTop(true)
    centerOnScreen()
    open()
  }

}