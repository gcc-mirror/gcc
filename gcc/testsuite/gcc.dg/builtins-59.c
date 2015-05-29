/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-require-effective-target libc_has_complex_functions } */

double test (double x)
{
  double s, c;
  __builtin_sincos (x, &s, &c);
  return s + c;
}

/* { dg-final { scan-tree-dump "__builtin_cexpi" "gimple" } } */
/* { dg-final { scan-tree-dump-not "sincos" "gimple" } } */
