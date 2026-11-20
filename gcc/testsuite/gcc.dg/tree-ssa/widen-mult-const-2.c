/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2 -fno-tree-reassoc -fdump-tree-optimized-raw" } */

__attribute__((noipa)) long long
multiply (int a, long long b)
{
  return (long long) (a * 30) * b * 7;
}

__attribute__((noipa)) long long
negative (int a, long long b)
{
  return (long long) (a * -30) * b * 7;
}

__attribute__((noipa)) long long
commuted (int a, long long b)
{
  return -7 * (b * (long long) (-30 * a));
}

/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [[:alnum:]_]*_[0-9]+(?:\(D\))?, [[:alnum:]_]*_[0-9]+(?:\(D\))?,} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<nop_expr, } 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times {gimple_return <} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times {gimple_[a-z_]+} 12 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 210,} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, -210,} 1 "optimized" } } */
