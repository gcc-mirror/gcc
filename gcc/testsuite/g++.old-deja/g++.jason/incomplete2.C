// { dg-do assemble  }

struct A {
  int foo(A a) { return a.bar(); }
  int bar();
  int n;
};
