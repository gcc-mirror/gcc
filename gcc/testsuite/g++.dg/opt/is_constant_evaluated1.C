// PR c++/108243
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O -fdump-tree-original" }

struct A {
  constexpr A(int n, int m) : n(n), m(m) { }
  int n, m;
};

constexpr int foo(int n) {
  return n + !__builtin_is_constant_evaluated();
}

A* f(int n) {
  static A a = {n, foo(41)};
  return &a;
}

// { dg-final { scan-tree-dump "42" "original" } }
// { dg-final { scan-tree-dump-not "foo \\(41\\)" "original" } }
