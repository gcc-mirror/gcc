/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

unsigned int
f1 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return x * 10;
}

unsigned int
f2 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 3;
  return x * 24;
}

/* { dg-final { scan-tree-dump-times {<rshift_expr,} 2 "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 10,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 24,} "optimized" } } */
