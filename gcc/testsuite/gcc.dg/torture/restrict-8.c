/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-options "-fdump-tree-fre1" } */

struct S { int i; void *p; int j; };
int
foo (struct S * __restrict p, int *q, int flag)
{
  int *x = &p->j;
  if (flag)
    x = &p->i;
  *q = 1;
  *x = 2;
  return *q;
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
