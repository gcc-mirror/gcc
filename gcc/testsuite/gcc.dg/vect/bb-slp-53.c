/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double a[2], b[2];

void foo(double x, double y)
{
  double breakme1 = y + 3.;
  double a1 = b[1] + 2.;
  double breakme0 = x;
  double a0 = b[0] + 1.;
  a[0] = a0 * breakme0;
  a[1] = a1 * breakme1;
}

/* We should vectorize the SLP opportunity starting from the
   grouped store to a[] including the load from b[] at the
   leaf even though the multiplication requires another
   vector invariant to be built.  */
/* { dg-final { scan-tree-dump "transform load" "slp2" } } */
