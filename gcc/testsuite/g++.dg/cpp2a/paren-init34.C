// PR c++/92812
// { dg-do compile { target c++20 } }
// Initializing arrays in a member init list using ()-init, invalid cases.

struct S { int x, y; };
struct N { int x, y; N(int, int); };

struct A {
  N a[2];
  A() : a(1, 2) { } // { dg-error "could not convert" }
};

struct B {
  S a[2];
  B() : a(1) // { dg-error "could not convert" }
    { }
};

// Copy-initialization does not consider explicit ctors.
struct E { explicit E(int); };

struct C {
  E a[2];
  C() : a(4, 5) { } // { dg-error "could not convert" }
};
