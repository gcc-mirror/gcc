// { dg-do assemble  }

struct S {
  typedef int I;
};

void f(typename S::I);
