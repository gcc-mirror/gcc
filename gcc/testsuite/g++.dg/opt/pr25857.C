/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo();
int i;

struct A
{
  ~A() { if (this != (A*)(&i)) foo(); }
};

struct B
{
  A a1, a2, a3, a4;
  ~B() { foo(); }
};

B b;
