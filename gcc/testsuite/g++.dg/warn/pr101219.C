/* PR c++/101219 - ICE on use of uninitialized memfun pointer
   { dg-do compile }
   { dg-options "-Wall" } */

struct S { void m(); };

template <int> bool f() {
  void (S::*mp)();

  return &S::m == mp; // no warning emitted here (no instantiation)
}
