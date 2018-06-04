/* { dg-options "-O2 -fdump-tree-widening_mul" } */

float
f1 (float a, float b, float c)
{
  return a * b - c;
}

double
f2 (double a, double b, double c)
{
  return a * b - c;
}

/* { dg-final { scan-tree-dump-times { = \.FMS \(} 2 "widening_mul" { target scalar_all_fma } } } */
