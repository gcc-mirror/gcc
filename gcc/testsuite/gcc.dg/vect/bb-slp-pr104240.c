/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_cond_mixed } */

void foo (int *c, float *x, float *y)
{
  c = __builtin_assume_aligned (c, __BIGGEST_ALIGNMENT__);
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  y = __builtin_assume_aligned (y, __BIGGEST_ALIGNMENT__);
  c[0] = x[0] < y[0];
  c[1] = y[1] > x[1];
  c[2] = x[2] < y[2];
  c[3] = x[3] < y[3];
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
