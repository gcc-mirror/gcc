// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized -Winit-self" }
// Test that we don't warn when initializing a reference, unless it's
// self-init.

struct R {
  int &r;
};

struct S {
  R r;
  int a;
  int &b;
  int c;
};

struct X {
  S s;
  X() : s{ { s.a }, 1, s.c, 3} { }
};

struct A {
  int &r;
  A() : r{r} { } // { dg-warning ".A::r. is initialized with itself" }
};

struct B {
  int &r;
  int a;
  B() : r{a} { }
};

struct C {
  R x;
  C() : x{x.r} { } // { dg-warning "member .C::x. is used uninitialized" }
};
