// { dg-do assemble  }
// { dg-options "-Wunused" }

struct A {
  int i:8;
  virtual ~A() {}
};
