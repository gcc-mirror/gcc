// PR c++/113760
// { dg-do compile }
// { dg-options "-Wextra-semi" }

struct S {
  void f1 () {}
  // A single semicolon is valid after a member function definition.
  void f2 () {}; // { dg-warning "extra .;. after in-class function definition" }
  void f3 () const;
};
void S::f3 () const { }; // { dg-warning "extra .;. outside of a function" }
