/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target ia32 } } */

/* The rule reaching vector code.  It matches the scalar loop body, so what is
   left to vectorize is a vector MAX or MIN of the unnegated loads and a vector
   add, in place of the vector negations and the vector subtract.  */

void
sub_min (int *r, int *q, int *a, int *x, int *y, int n)
{
  for (int i = 0; i < n; i++)
    {
      int nx = -x[i], ny = -y[i];
      int m = nx < ny ? nx : ny;
      q[i] = nx;
      r[i] = a[i] - m;
    }
}

void
sub_max (int *r, int *q, int *a, int *x, int *y, int n)
{
  for (int i = 0; i < n; i++)
    {
      int nx = -x[i], ny = -y[i];
      int m = nx > ny ? nx : ny;
      q[i] = nx;
      r[i] = a[i] - m;
    }
}

void
sub_min_cst (int *r, int *a, int *x, int n)
{
  for (int i = 0; i < n; i++)
    {
      int nx = -x[i];
      int m = nx < 5 ? nx : 5;
      r[i] = a[i] - m;
    }
}

/* { dg-final { scan-tree-dump-not "= vect_\[^;\]* - vect_" "optimized" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR <vect_\[^,\]*, \{ 5" "optimized" } } */
/* { dg-final { scan-tree-dump "= vect_\[^;\]* \\+ vect_" "optimized" { target { ! vect_no_int_min_max } } } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <vect_\[^,\]*, \{ -5" "optimized" { target { ! vect_no_int_min_max } } } } */
