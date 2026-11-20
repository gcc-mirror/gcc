/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

volatile long long sink;

long long
mismatch (int a, long long b)
{
  return (long long) (a * 30) * b / 7;
}

long long
shared (int a, long long b)
{
  long long x = (long long) (a * 30);
  sink = x;
  return x * b / 6;
}

long long
unsigned_source (unsigned int a, long long b)
{
  return (long long) (a * 30U) * b / 6;
}

long long
constant_overflow (int a, long long b)
{
  return (long long) (a * 2) * b * 4611686018427387904LL;
}

/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 30,} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 2,} 1 "optimized" } } */
