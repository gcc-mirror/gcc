// PR c++/18530
// { dg-options "-Wshadow" }

struct A {
  A();
  ~A();
  void foo (int __ct, int __dt) {}
};
