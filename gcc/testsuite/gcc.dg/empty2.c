/* { dg-do compile } */

double d=0;

struct A {} a;

void foo(struct A x)
{
  d=0;
}

void bar()
{
  if (d) foo(a);
}
