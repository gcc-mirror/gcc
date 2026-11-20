/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized-raw" } */

long long
divide (int a, long long b)
{
  return (long long) (a * 30) * b / 6;
}

long long
multiply (int a, long long b)
{
  return (long long) (a * 30) * b * 7;
}

/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 30,} 2 "optimized" } } */
