// { dg-options "-Wabi" }

struct A {};

struct B {
  A a; // { dg-warning "empty" }
  virtual void f () {}
};
