/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-tree-ompexp" } */
/* { dg-additional-options "-march=pentium" { target ia32 } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 8, 5, 5\\\);" 1 "ompexp" { target sync_long_long } } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 8, 4, 2\\\);" 1 "ompexp" { target sync_long_long } } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 264, 5, 0\\\);" 1 "ompexp" { target sync_long_long } } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 8, 0, 0\\\);" 1 "ompexp" { target sync_long_long } } } */
/* { dg-final { scan-tree-dump-not "__atomic_load_8 \\\(" "ompexp" { target sync_long_long } } } */

double x;

void
foo (double y, double z)
{
  #pragma omp atomic compare seq_cst
  x = x == y ? z : x;
}

double
bar (double y, double z)
{
  int r;
  #pragma omp atomic compare capture acq_rel fail (acquire)
  { r = x == y; if (r) { x = z; } }
  return r;
}

double
baz (double y, double z)
{
  double v;
  #pragma omp atomic compare capture seq_cst fail (relaxed) weak
  if (x == y) { x = z; } else { v = x; }
  return v;
}

double
qux (double y, double z)
{
  double v;
  #pragma omp atomic compare capture
  v = x = x == y ? z : x;
  return v;
}
