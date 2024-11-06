/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

unsigned int
f1 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return x << 3;
}

unsigned int
f2 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  unsigned char y = x;
  y >>= 2;
  return y << 3;
}

unsigned long
f3 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return (unsigned long) x << 3;
}

int
f4 (int x)
{
  if (x & 15)
    __builtin_unreachable ();
  x >>= 4;
  return x << 5;
}

unsigned int
f5 (int x)
{
  if (x & 31)
    __builtin_unreachable ();
  x >>= 5;
  return x << 6;
}

unsigned int
f6 (unsigned int x)
{
  if (x & 1)
    __builtin_unreachable ();
  x >>= 1;
  return x << (sizeof (int) * __CHAR_BIT__ - 1);
}

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-times {<lshift_expr, [^,]*, [^,]*, 1,} 5 "optimized" } } */
/* { dg-final { scan-tree-dump {<lshift_expr, [^,]*, [^,]*, 30,} "optimized" { target int32 } } } */
