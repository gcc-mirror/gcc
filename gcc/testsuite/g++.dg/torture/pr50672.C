// { dg-do compile }

struct A
{
  bool isHint();
};
class B
{
  void makeLine( int *) const;
  void drawLine() const; A* to() const;
  void _print() const;
};
A a;
void  B::makeLine(int *p1) const
{
  if (a.isHint() && to()->isHint()) ;
  else {
      if (p1) B::drawLine(); else B::_print();
      return;
  }
  if (p1) B::drawLine(); else B::_print();
}
