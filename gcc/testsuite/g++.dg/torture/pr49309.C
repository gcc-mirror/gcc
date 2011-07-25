// PR tree-optimization/49309
// { dg-do compile }
// { dg-options "-fpreprocessed -fmudflap" }

struct A
{
  int i;

  A();
  A(const A&);
};

inline void foo(A a) { a = A(); }

void bar() { foo(A()); }
