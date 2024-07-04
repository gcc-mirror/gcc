// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized -Winit-self" }
// Largely copied from clang's test/SemaCXX/uninitialized.cpp.

int x;
struct U {
  U() : b(a) { }
  int &a = x;
  int &b;
};

struct T {
  T() : a(b), b(a) { } // { dg-warning "reference .T::b. is not yet bound" }
  int &a, &b;
};

struct S {
  S() : a(a) { } // { dg-warning ".S::a. is initialized with itself" }
  int &a;
};

struct A {
  int a;
  int b;
  A() { }
  A(int (*)[1]) : b(a) { } // { dg-warning ".A::a. is used uninitialized" }
  A(int (*)[2]) : a(b) { } // { dg-warning ".A::b. is used uninitialized" }
};

struct D {
  int a;
  int &b;
  int &c = a;
  int d = b;
  D() : b(a) { }
};

struct E {
  int a;
  int get();
  static int num();
  E() { }
  E(int) { }
};

struct F {
  int a;
  E e;
  int b;
  F(int (*)[1]) : a(e.get()) { } // { dg-warning "member .F::e. is used uninitialized" }
  F(int (*)[2]) : a(e.num()) { }
  F(int (*)[3]) : e(a) { } // { dg-warning "member .F::a. is used uninitialized" }
  F(int (*)[4]) : a(4), e(a) { }
  F(int (*)[5]) : e(b) { } // { dg-warning "member .F::b. is used uninitialized" }
  F(int (*)[6]) : e(b), b(4) { } // { dg-warning "member .F::b. is used uninitialized" }
};

struct G {
  G(const A&) { };
};

struct H {
  A a1;
  G g;
  A a2;
  H() : g(a1) { }
  H(int) : g(a2) { } // { dg-bogus "member .H::a2. is used uninitialized" }
};

struct I {
  I(int *) { }
};

struct J : I {
  int *a;
  int *b;
  int c;
  J() : I((a = new int(5))), b(a), c(*a) { }
};

struct M { };

struct N : public M {
  int a;
  int b;
  N() : b(a) { } // { dg-warning "member .N::a. is used uninitialized" }
};

struct O {
  int x = 42;
  int get() { return x; }
};

struct P {
  O o;
  int x = o.get();
  P() : x(o.get()) { }
};

struct Q {
  int a;
  int b;
  int &c;
  Q() :
    a(c = 5), // "reference .Q::c. is not yet bound" but too complex for the FE
    b(c), // "reference .Q::c. is not yet bound" but too complex for the FE
    c(a) { }
};

struct R {
  int a;
  int b;
  int c;
  int d = a + b + c;
  R() : a(c = 5), b(c), c(a) { }
};
