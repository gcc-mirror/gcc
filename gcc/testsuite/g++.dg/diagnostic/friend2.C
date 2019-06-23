// PR c++/61490
// { dg-do compile }

namespace N { void f (); }
void f2 ();

struct A {
  friend void N::f () { } // { dg-error "15:friend function definition 'f' cannot have a name qualified with 'N::'" }
  friend void ::f2 () { } // { dg-error "15:friend function definition 'f2' cannot have a name qualified with '::'" }
};
