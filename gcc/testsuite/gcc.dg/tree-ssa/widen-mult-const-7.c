/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2 -fgimple -fno-tree-reassoc -fdump-tree-optimized-raw" } */

long long __GIMPLE
cancel_exact (int a, long long b)
{
  int narrow;
  long long wide;
  long long product;
  long long result;

  narrow = a * 65536;
  wide = (long long) narrow;
  product = wide * b;
  result = product __EXACT_DIV 65536LL;
  return result;
}

long long __GIMPLE
partial_exact (int a, long long b)
{
  int narrow;
  long long wide;
  long long product;
  long long result;

  narrow = a * 30;
  wide = (long long) narrow;
  product = wide * b;
  result = product __EXACT_DIV 6LL;
  return result;
}

/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [[:alnum:]_]*_[0-9]+(?:\(D\))?, [[:alnum:]_]*_[0-9]+(?:\(D\))?,} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<nop_expr, } 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {gimple_return <} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {gimple_[a-z_]+} 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 5,} 1 "optimized" } } */
