/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_cond_mixed } */

void foo (int *c, float *x, float *y)
{
  c[0] = x[0] < y[0];
  c[1] = y[1] > x[1];
  c[2] = x[2] < y[2];
  c[3] = x[3] < y[3];
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
