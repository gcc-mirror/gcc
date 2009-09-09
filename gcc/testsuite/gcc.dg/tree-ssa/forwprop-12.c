/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

struct X { int a[256]; };

int foo(struct X *p, __SIZE_TYPE__ i)
{
  int *q = &p->a[0];
  int *q2 = (int *)((void *)q + i*4 + 32);
  return *q2;
}

int bar(struct X *p, int i)
{
  return *((int *)p + i + 8);
}

/* We should have propagated the base array address through the
   address arithmetic into the memory access as an array access.  */

/* { dg-final { scan-tree-dump-times "->a\\\[D\\\." 2 "forwprop1" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
