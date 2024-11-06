/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

unsigned int
f1 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 3;
  return x << 4;
}

unsigned int
f2 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return x << 1;
}

/* { dg-final { scan-tree-dump-times {<rshift_expr,} 2 "optimized" } } */
