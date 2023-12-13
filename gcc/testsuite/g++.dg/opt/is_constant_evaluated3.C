// PR c++/108243
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O -fdump-tree-original" }

struct A {
  constexpr A(int n) : n(n), m(__builtin_is_constant_evaluated()) { }
  constexpr A() : A(42) { }
  int n, m;
};

int main() {
  A a1 = {42};
  A a2{42};
  A a3(42);
  A a4;
  A a5{};
}

// { dg-final { scan-tree-dump "a1 = {\\.n=42, \\.m=0}" "original" } }
// { dg-final { scan-tree-dump "a2 = {\\.n=42, \\.m=0}" "original" } }
// { dg-final { scan-tree-dump "a3 = {\\.n=42, \\.m=0}" "original" } }
// { dg-final { scan-tree-dump "a4 = {\\.n=42, \\.m=0}" "original" } }
// { dg-final { scan-tree-dump "a5 = {\\.n=42, \\.m=0}" "original" } }
