// { dg-do compile }
// { dg-options "-fabi-version=0" }

struct A {
  template <typename T> int f ();
};

typedef int (A::*P)();

template <P> struct S {};

void g (S<&A::f<int> >) {}

// { dg-final { scan-assembler _Z1g1SIXadL_ZN1A1fIiEEivEEE } }
