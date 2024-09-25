/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */
/* { dg-additional-options "-fdump-tree-optimized" } */

void foo (int * __restrict a, int *b, int *c)
{
  for (int i = 0; i < 1024; ++i)
    {
      a[2*i] = b[i] + 7;
      a[2*i+1] = c[i] * 3;
    }
}

int bar (int *b)
{
  int res = 0;
  for (int i = 0; i < 1024; ++i)
    {
      res += b[2*i] + 7;
      res += b[2*i+1] * 3;
    }
  return res;
}

void baz (int * __restrict a, int *b)
{
  for (int i = 0; i < 1024; ++i)
    {
      a[2*i] = b[2*i] + 7;
      a[2*i+1] = b[2*i+1] * 3;
    }
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "LOAD_LANES" 2 "optimized" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump-times "STORE_LANES" 2 "optimized" { target vect_load_lanes } } } */
