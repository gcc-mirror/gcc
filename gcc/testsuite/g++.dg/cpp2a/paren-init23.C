// PR c++/93790 - wrong paren-init of aggregates interference.
// { dg-do compile { target c++2a } }

struct S {
  int i;
};
const S& s(1);

struct A {
  int i;
  A(int);
};
const A& a(1);

struct B {
  int i;
  B(int) = delete;
};
const B& b(1); // { dg-error "use of deleted function" }
