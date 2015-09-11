/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1-details" } */

struct S { float f; };
int __attribute__((noinline))
foo (float *r, struct S *p)
{
  int *q = (int *)&p->f;
  int i = *q;
  *r = 0.0;
  return i + *q;
}

/* { dg-final { scan-tree-dump "Replaced\[^\n\]*with i_." "fre1" } } */
