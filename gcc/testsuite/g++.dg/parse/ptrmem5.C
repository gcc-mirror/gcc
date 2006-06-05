// PR c++/27806

struct A {};

void foo()
{
  p;  // { dg-error "p" }
  extern int A::* p;
}
