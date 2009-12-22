// { dg-do compile }
// { dg-options "-Wabi -fabi-version=1" }
// { dg-final { scan-assembler "_Z1g1SIXadsr1ANS0_1fIiEEivEE" } }

struct A {
  template <typename T> int f ();
};

typedef int (A::*P)();

template <P> struct S {};

void g (S<&A::f<int> >) {} // { dg-warning "mangle" }
