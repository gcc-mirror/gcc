/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-vect-details -mprefer-vector-width=256" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */

#define N 10000

void
__attribute__((noipa))
foo_neg (int* a, int b)
{
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b = -b;
    }
}

void
__attribute__((noipa))
foo_neg_const (int* a)
{
  int b = 1;
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b = -b;
    }
}

void
__attribute__((noipa))
foo_neg_peel (int* a, int b, int n)
{
  for (int i = 0; i != n; i++)
    {
      a[i] = b;
      b = -b;
    }
}

void
__attribute__((noipa))
foo_neg_const_peel (int* a, int n)
{
  int b = 1;
  for (int i = 0; i != n; i++)
    {
      a[i] = b;
      b = -b;
    }
}
