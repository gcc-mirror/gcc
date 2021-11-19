// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }

struct A {
  int m;
  int get() const { return m; }

  A() : m{} { }
  A(int) { }
  A(const A &) { }
  A(A *) { }
};

struct S {
  A a, b;

  S(int (*)[1]) : a() {}
  S(int (*)[2]) : b(a.get()) {}
  S(int (*)[3]) : b(a) {}
  S(int (*)[4]) : a(&a) {}
};

struct R {
  A a, b;

  R(int (*)[1]) : a{} {}
  R(int (*)[2]) : b{a.get()} {}
  R(int (*)[3]) : b{a} {}
  R(int (*)[4]) : a{&a} {}
};
