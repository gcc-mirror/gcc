// PR c++/51868
// { dg-options -std=c++0x }

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
