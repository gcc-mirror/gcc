// PR c++/50234
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

struct A { int i; };

constexpr int f(A a) { return a.i; }

SA(f({}) == 0);
