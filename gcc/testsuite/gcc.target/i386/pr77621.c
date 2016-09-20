/* { dg-do compile } */
/* { dg-options "-O3 -mtune=atom -msse2 -fdump-tree-vect-stats" } */

void
foo (double *x, int *y)
{
  int i;
  for (i = 0; i < 8; i++)
    x[i] -= y[i] * x[i + 1];
}

/* { dg-final { scan-tree-dump-not "Vectorized loops: 1" "vect" } } */
