/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate" } */

struct S
{
  int a;
  void **b;
};

void
foo (struct S *x, int y)
{
  if (!x)
    return;
  if (y >= x->a)
    return;
  x->a--;
  for (; y < x->a; y++)
    x->b[y] = x->b[y + 1];
  x->b[x->a] = (void *) 0;
}

/* { dg-final { cleanup-coverage-files } } */
