/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-vect-details -mprefer-vector-width=256" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */

#define N 10000

void
__attribute__((noipa))
foo_mul (int* a, int b)
{
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b *= 3;
    }
}

void
__attribute__((noipa))
foo_mul_const (int* a)
{
  int b = 1;
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b *= 3;
    }
}

void
__attribute__((noipa))
foo_mul_peel (int* a, int b)
{
  for (int i = 0; i != 39; i++)
    {
      a[i] = b;
      b *= 3;
    }
}

void
__attribute__((noipa))
foo_mul_peel_const (int* a)
{
  int b = 1;
  for (int i = 0; i != 39; i++)
    {
      a[i] = b;
      b *= 3;
    }
}
