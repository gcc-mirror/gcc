// PR middle-end/14477
// { dg-do compile }
// { dg-options "-O2 -fno-default-inline" }

struct A
{
  A();
};

struct B
{
  B(const A*);
};

struct C
{
  B b;
  C(int) : b(new A) {}
};

C c(0);
