/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double dotprod(const double *a, const double *b, unsigned long long n)
{
  double d1 = 0.0;
  double d2 = 0.0;

  for (unsigned long long i = 0; i < n; i += 2) {
    d1 += a[i] * b[i];
    d2 += a[i + 1] * b[i + 1];
  }

  return (d1 + d2);
}

/* We should use a SLP reduction even without -ffast-math by using a
   VF of one.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { xfail vect_variable_length } } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
