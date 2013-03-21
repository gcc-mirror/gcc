// PR c++/54532
// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X,#X)

struct A {
  int i;
  constexpr static int A::*p = &A::i;
};

constexpr A a = { 42 };
SA(a.*A::p == 42);

constexpr int A::* A::p;
