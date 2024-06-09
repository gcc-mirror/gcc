/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-vect-cost-model -ffast-math -fdump-tree-optimized-details" } */

void
foo (int n, int *__restrict in, int *__restrict out)
{
  for (int i = 0; i < n; i += 1)
    {
      out[i] = in[i] + i;
    }
}

void
foo2 (int n, float * __restrict in, 
float * __restrict out)
{
  for (int i = 0; i < n; i += 1)
    {
      out[i] = in[i] + i;
    }
}

void
foo3 (int n, float * __restrict in, 
float * __restrict out, float x)
{
  for (int i = 0; i < n; i += 1)
    {
      out[i] = in[i] + i* i;
    }
}

/* We don't want to see vect_vec_iv_.21_25 + { POLY_INT_CST [4, 4], ... }.  */
/* { dg-final { scan-tree-dump-not "\\+ \{ POLY_INT_CST" "optimized" } } */
