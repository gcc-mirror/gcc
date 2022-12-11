/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

struct b { int data[16]; };

int foo (struct b *x)
{
  int *a = x->data;
  int *b = ((int*)x) + 4;
  return b - a;
}

/* { dg-final { scan-tree-dump "return 4;" "ccp1" } } */
