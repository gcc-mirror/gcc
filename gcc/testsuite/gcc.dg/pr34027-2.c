/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

long foo(long n, long m)
{
  return n - (n / m) * m;
}

/* { dg-final { scan-tree-dump "n % m" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
