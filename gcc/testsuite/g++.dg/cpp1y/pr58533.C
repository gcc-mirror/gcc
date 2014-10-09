// PR c++/58533
// { dg-do compile { target c++14 } }

void foo()
{
  void (*fp)(auto); // { dg-error "auto|not permitted" }
}
