/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

struct b { int data[16]; };

int foo (struct b *x)
{
  int *a = x->data;
  int *b = ((int*)x) + 4;
  return b - a;
}

int bar (struct b *x)
{
  int *a = x->data;
  int *b = ((int*)x) + 4;
  if (a != b)
    return 1;
  return 0;
}

int baz (struct b *x)
{
  int *a = x->data;
  int *b = ((int*)x) + 4;
  return (a != b);
}

/* { dg-final { scan-tree-dump "return 4;" "ccp1" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "ccp1" } } */
