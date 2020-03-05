// PR c++/91925
// { dg-do compile { target c++11 } }
// { dg-options "-fpack-struct" }

struct A {};
int foo (A);
struct B {
  A a;
  decltype (foo (a)) p;
};
template <typename T> T bar (T);
class C {
  A a;
  decltype (bar (a)) p;
};
