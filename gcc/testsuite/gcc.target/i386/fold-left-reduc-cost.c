/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -fdump-tree-vect-details" } */

/* The byte loads make V32QI available, but the fold-left reduction should
   make the smaller SSE loop win.  */

float
foo (char *a, char *b, int n)
{
  float sum = 0;
  for (int i = 0; i != n; i++)
    sum += a[i] * b[i];
  return sum;
}

/* { dg-final { scan-tree-dump "in-order FP reduction lanes" "vect" } } */
/* { dg-final { scan-tree-dump "loop vectorized using 16 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump-not "loop vectorized using 32 byte vectors" "vect" } } */
