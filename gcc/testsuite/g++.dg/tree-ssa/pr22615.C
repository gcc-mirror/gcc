/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Ensure that we don't crash when people decide to return the address of padding.  */

struct A
{
    char c;
      int i;
};

A a;

struct B
{
    char c, d;
};

union C
{
    A *p;
      B *q;

        C() : p(&a) {}
	  char& foo() { return q->d; }
};
void bar() { C().foo() = 0; }

