/* { dg-options "-O2 -fdump-tree-widening_mul" } */

float
f1 (float a, float b, float c)
{
  return c - a * b;
}

double
f2 (double a, double b, double c)
{
  return c - a * b;
}

/* { dg-final { scan-tree-dump-times { = \.FNMA \(} 2 "widening_mul" { target scalar_all_fma } } } */
