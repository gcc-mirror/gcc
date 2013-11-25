// PR c++/58533
// { dg-options "-std=gnu++1y" }

void foo()
{
  void (*fp)(auto); // { dg-error "auto|not permitted" }
}
