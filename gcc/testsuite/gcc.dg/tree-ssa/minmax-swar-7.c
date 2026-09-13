/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target ia32 } } */

/* MINMAX (X + Y, Y) - Y through the loop vectorizer: the vector add and the
   vector subtract both go, leaving the clamp against zero.  */

void
clamp_lo (int *r, int *x, int *y, int n)
{
  for (int i = 0; i < n; i++)
    {
      int s = x[i] + y[i];
      r[i] = (s < y[i] ? s : y[i]) - y[i];
    }
}

void
clamp_hi (int *r, int *x, int *y, int n)
{
  for (int i = 0; i < n; i++)
    {
      int s = x[i] + y[i];
      r[i] = (s > y[i] ? s : y[i]) - y[i];
    }
}

/* { dg-final { scan-tree-dump-not "= vect_\[^;\]* - vect_" "optimized" } } */
/* { dg-final { scan-tree-dump-not "= vect_\[^;\]* \\+ vect_" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <vect_\[^,\]*, \{ 0" "optimized" { target { ! vect_no_int_min_max } } } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <vect_\[^,\]*, \{ 0" "optimized" { target { ! vect_no_int_min_max } } } } */
