// PR c++/51868
// { dg-do compile { target c++11 } }

struct A {
  A() {}
  A(const A&) {}
  A(A&&) {}
};

struct B {
  A a;
  int f : 1;
};

B func() {
  return B();
}
