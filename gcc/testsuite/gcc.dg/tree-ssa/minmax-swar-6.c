/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target ia32 } } */

/* The two difference pairs reaching vector code through the loop vectorizer:
   the fold is on the scalar loop body, so what is left to vectorize is a
   vector MIN or MAX of the two loads, with no vector add or subtract.  */

void
add_min (int *r, int *a, int *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] - b[i];
      r[i] = b[i] + (t < 0 ? t : 0);
    }
}

void
add_max (int *r, int *a, int *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] - b[i];
      r[i] = b[i] + (t > 0 ? t : 0);
    }
}

void
sub_min (int *r, int *a, int *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] - b[i];
      r[i] = a[i] - (t < 0 ? t : 0);
    }
}

void
sub_max (int *r, int *a, int *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] - b[i];
      r[i] = a[i] - (t > 0 ? t : 0);
    }
}

/* { dg-final { scan-tree-dump-not "= vect_\[^;\]* - vect_" "optimized" } } */
/* { dg-final { scan-tree-dump-not "= vect_\[^;\]* \\+ vect_" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <vect_\[^,\]*, vect_" "optimized" { target { ! vect_no_int_min_max } } } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <vect_\[^,\]*, vect_" "optimized" { target { ! vect_no_int_min_max } } } } */
