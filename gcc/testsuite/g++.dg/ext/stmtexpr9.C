// PR c++/28899
// { dg-options "" }

void f()
{
  unsigned l, l1;
  l1 = l = ({ unsigned __v; __v; });
}
