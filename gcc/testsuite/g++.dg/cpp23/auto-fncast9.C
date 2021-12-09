// PR c++/103401
// { dg-do compile { target c++23 } }

void f1(decltype(new auto{0}));
void f2(decltype(new int{0}));

void
g ()
{
  int i;
  void f3(decltype(new auto{0}));
  void f4(decltype(new int{0}));
  f1 (&i);
  f2 (&i);
  f3 (&i);
  f4 (&i);
}
