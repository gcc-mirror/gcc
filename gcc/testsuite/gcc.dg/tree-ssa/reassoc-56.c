/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-reassoc1" } */

double pow (double, double);

double
f (double x, double y, double z)
{
  return (x * y) * __builtin_powi (x, 8000) + x * z * x * pow (x, -8000.0);
}

/* Un-distribution of x must not strip x from pow (x, -8000.0) and
   iterate until the __builtin_powi exponent is exhausted.  */
/* { dg-final { scan-tree-dump "pow \\(x_\[0-9\]+\\(D\\), -8\\.0e\\+3\\)" "reassoc1" } } */
/* { dg-final { scan-tree-dump-times " \\* " 4 "reassoc1" } } */
