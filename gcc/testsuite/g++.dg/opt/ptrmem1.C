// { dg-options "-O2" }

class QWidget
{
  public: void repaint( bool erase  = 1 );
};
void f (void)
{
  typedef void (QWidget::*A)(bool);
  A b = &QWidget::repaint;
}
 
