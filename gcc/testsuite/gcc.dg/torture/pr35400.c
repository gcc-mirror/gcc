/* { dg-do compile } */
/* { dg-options "-Wtype-limits" } */

struct A
{
  struct A *p;
};

int foo(const struct A *q)
{
  return q->p == q;
}

void bar(int);

void baz()
{
  struct A a;

  while (foo(&a))
    bar(foo(&a));
}
