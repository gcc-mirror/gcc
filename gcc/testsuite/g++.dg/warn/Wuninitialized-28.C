// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }

struct S {
  int i, j, k, l;
  S() : i(j), // { dg-warning "member .S::j. is used uninitialized" }
	j(1),
	k(l + 1), // { dg-warning "member .S::l. is used uninitialized" }
	l(2) { }
};

struct A {
  int a, b, c;
  A() : a(b // { dg-warning "member .A::b. is used uninitialized" }
	  + c) { } // { dg-warning "member .A::c. is used uninitialized" }
};

struct B {
  int &r;
  int *p;
  int a;
  B() : r(a), p(&a), a(1) { }
};

struct C {
  const int &r1, &r2;
  C () : r1(r2), // { dg-warning "reference .C::r2. is not yet bound to a value when used here" }
	 r2(r1) { }
};

struct D {
  int a = 1;
  int b = 2;
  D() : a(b + 1), b(a + 1) { } // { dg-warning "member .D::b. is used uninitialized" }
};

struct E {
  int a = 1;
  E() : a(a + 1) { } // { dg-warning "member .E::a. is used uninitialized" }
};

struct F {
  int a = 1;
  int b;
  F() : b(a + 1) { }
};

struct bar {
  int a;
  bar() {}
  bar(bar&) {}
};

class foo {
  bar first;
  bar second;
public:
  foo() : first(second) {} // { dg-warning "member .foo::second. is used uninitialized" }
};
