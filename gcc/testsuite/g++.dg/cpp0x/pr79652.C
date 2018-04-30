// { dg-do compile { target c++11 } }

namespace {
  struct S {
    int n;
  } T;
  using T = decltype(S :: n);  // { dg-error "redeclared" }
  namespace {
    extern int x;
    extern int x;
  }
}
