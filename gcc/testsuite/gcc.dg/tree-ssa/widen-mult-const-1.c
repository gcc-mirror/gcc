/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2 -fno-tree-reassoc -fdump-tree-optimized-raw" } */

__attribute__((noipa)) long long
cancel (int a, long long b)
{
  return (long long) (a * 65536) * b / 65536;
}

__attribute__((noipa)) long long
partial (int a, long long b)
{
  return (long long) (a * 30) * b / 6;
}

__attribute__((noipa)) long long
negative (int a, long long b)
{
  return (long long) (a * -30) * b / 6;
}

__attribute__((noipa)) long long
commuted (int a, long long b)
{
  return b * (long long) (-30 * a) / -30;
}

/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [[:alnum:]_]*_[0-9]+(?:\(D\))?, [[:alnum:]_]*_[0-9]+(?:\(D\))?,} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<nop_expr, } 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times {gimple_return <} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times {gimple_[a-z_]+} 14 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 5,} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, -5,} 1 "optimized" } } */
