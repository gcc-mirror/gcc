// PR c++/96121
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }
// Test we warn with delegating constructors.

struct A {
  A(int);
  A(int &, int);
  A(int (*)[1]) : A(x) { } // { dg-warning "21:member .A::x. is used uninitialized" }
  A(int (*)[2]) : A(x, x) { } // { dg-warning "24:member .A::x. is used uninitialized" }
  A(int (*)[3]) : A(x, 0) { }
  A(int (*)[4]) : A{x} { } // { dg-warning "21:member .A::x. is used uninitialized" }
  A(int (*)[5]) : A{x, x} { } // { dg-warning "24:member .A::x. is used uninitialized" }
  A(int (*)[6]) : A{x, 0} { }
  int x;
};
