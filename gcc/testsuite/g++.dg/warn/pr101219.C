/* PR c++/101219 - ICE on use of uninitialized memfun pointer
   { dg-do compile }
   { dg-options "-Wall" } */

struct S { void m(); };

template <int> bool f() {
  void (S::*mp)();

  /* The expression below isn't type-dependent so also verify
     it's diagnosed even though the template isn't instantiated.  */
  return &S::m == mp; // { dg-warning "\\\[-Waddress" }
}
