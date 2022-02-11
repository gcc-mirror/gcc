// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }
// Anonymous union/struct.
// ??? The diagnostic should be improved to say 'b' instead of
// "<anonymous>".

struct S {
  __extension__ struct {
    int a;
    int b;
  };
  S() : a(b) { } // { dg-warning "is used uninitialized" }
};

struct U {
  union {
    int a;
    int b;
  };
  U() : a(b) { } // { dg-warning "is used uninitialized" }
};
