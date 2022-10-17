// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }
// Test we warn when initializing a base class.

struct A {
  A(int) { }
};

struct B : public A {
  int x;
  B() : A(x) { } // { dg-warning "member .B::x. is used uninitialized" }
};

struct C : public A {
  int x;
  int y;
  C() : A(y = 4), x(y) { }
};

struct D : public A {
  int x;
  D() : A{x} { } // { dg-warning "member .D::x. is used uninitialized" }
};

struct E : public A {
  int x;
  int y;
  E() : A{y = 4}, x(y) { }
};

struct F {
  F(int&) { }
};

struct G : F {
  int x;
  G() : F(x) { }
};

struct H {
  H(int *) { }
};

struct I : H {
  int x;
  int arr[2];
  I() : H(&x) { }
  I(int) : H(arr) { }
};
