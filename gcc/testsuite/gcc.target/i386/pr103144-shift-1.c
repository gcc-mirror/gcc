/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-vect-details -mprefer-vector-width=256" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 6 "vect" } } */

#define N 10000
void
__attribute__((noipa))
foo_shl (int* a, int b)
{
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b <<= 1;
    }
}

void
__attribute__((noipa))
foo_ashr (int* a, int b)
{
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b >>= 1;
    }
}

void
__attribute__((noipa))
foo_lshr (unsigned int* a, unsigned int b)
{
  for (int i = 0; i != N; i++)
    {
      a[i] = b;
      b >>= 1U;
    }
}

void
__attribute__((noipa))
foo_shl_peel (int* a, int b)
{
  for (int i = 0; i != 39; i++)
    {
      a[i] = b;
      b <<= 1;
    }
}

void
__attribute__((noipa))
foo_ashr_peel (int* a, int b)
{
  for (int i = 0; i != 39; i++)
    {
      a[i] = b;
      b >>= 1;
    }
}

void
__attribute__((noipa))
foo_lshr_peel (unsigned int* a, unsigned int b)
{
  for (int i = 0; i != 39; i++)
    {
      a[i] = b;
      b >>= 1U;
    }
}
