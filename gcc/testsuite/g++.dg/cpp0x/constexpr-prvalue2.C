// PR c++/117980
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

struct S {
  constexpr S(S &); // { dg-warning "used but never defined" }
  ~S();
};
struct B {
  S s;
};
struct A {
  B b;
};
void fn(B b) { A{b}; }
