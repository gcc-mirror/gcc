/* PR tree-optimization/114322 */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int
foo (int *A, int *B, int x, int stride)
{
  int sum = 0;

  if (stride > 1)
    {
      for (int i = 0; i < 1024; ++i)
        sum += A[(i + x) * stride] + B[i];
    }

  return sum;
}

/* { dg-final { scan-tree-dump-not "failed: evolution of base is not affine." "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
