/* { dg-do compile } */
/* { dg-options "-fmudflap"  } */
struct A
{
  int i;

  A();
  A(const A&);
};

inline void foo(A a) { a = A(); }

void bar() { foo(A()); }

