// { dg-do compile }
// Origin: C++ Standard Draft (7.3.3/12)
// PR c++/2294: using declarations should not conflict, but only cause
//  an ambiguous overload set to be created.

namespace B {
  void f(int);     // { dg-error "" }
  void f(double);  // { dg-error "" }
}

namespace C {
  void f(int);     // { dg-error "" }
  void f(double);  // { dg-error "" }
  void f(char);    // { dg-error "" }
}

void h()
{
  using B::f;
  using C::f;
  f('h');
  f(1);         // { dg-error "ambiguous" }
  void f(int);  // { dg-error "previous using declaration" }
}

void m()
{
  void f(int);
  using B::f;   // { dg-error "already declared" }
}
