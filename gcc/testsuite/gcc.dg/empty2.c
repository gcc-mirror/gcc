/* { dg-do compile } */

double d=0;

struct A {} a; /* { dg-warning "(has no members)" } */

void foo(struct A x)
{
  d=0;
}

void bar()
{
  if (d) foo(a);
}
