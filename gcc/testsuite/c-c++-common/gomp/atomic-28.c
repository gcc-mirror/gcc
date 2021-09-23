/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-tree-ompexp" } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 4, 5, 5\\\);" 1 "ompexp" { target sync_int_long } } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 4, 4, 2\\\);" 1 "ompexp" { target sync_int_long } } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 260, 5, 0\\\);" 1 "ompexp" { target sync_int_long } } } */
/* { dg-final { scan-tree-dump-times "\.ATOMIC_COMPARE_EXCHANGE \\\(\[^\n\r]*, 4, 0, 0\\\);" 1 "ompexp" { target sync_int_long } } } */
/* { dg-final { scan-tree-dump-not "__atomic_load_4 \\\(" "ompexp" { target sync_int_long } } } */

int x;

void
foo (int y, int z)
{
  #pragma omp atomic compare seq_cst
  x = x == y ? z : x;
}

int
bar (int y, int z)
{
  int r;
  #pragma omp atomic compare capture acq_rel fail (acquire)
  { r = x == y; if (r) { x = z; } }
  return r;
}

int
baz (int y, int z)
{
  int v;
  #pragma omp atomic compare capture seq_cst fail (relaxed) weak
  if (x == y) { x = z; } else { v = x; }
  return v;
}

int
qux (int y, int z)
{
  int v;
  #pragma omp atomic compare capture
  v = x = x == y ? z : x;
  return v;
}
