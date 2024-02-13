// PR c++/113760
// { dg-do compile }
// { dg-options "-pedantic-errors -Wno-extra-semi" }

struct S {
  void f1 () {}
  // A single semicolon is valid after a member function definition.
  void f2 () {};
  void f3 () const;
};
void S::f3 () const { };
