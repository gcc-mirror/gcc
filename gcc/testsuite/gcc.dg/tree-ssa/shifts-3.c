/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

unsigned int
f1 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return x * 20;
}

unsigned int
f2 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  unsigned char y = x;
  y >>= 2;
  return y * 36;
}

unsigned long
f3 (unsigned int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return (unsigned long) x * 88;
}

int
f4 (int x)
{
  if (x & 15)
    __builtin_unreachable ();
  x >>= 4;
  return x * 48;
}

unsigned int
f5 (int x)
{
  if (x & 31)
    __builtin_unreachable ();
  x >>= 5;
  return x * 3200;
}

unsigned int
f6 (unsigned int x)
{
  if (x & 1)
    __builtin_unreachable ();
  x >>= 1;
  return x * (~0U / 3 & -2);
}

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 5,} "optimized" } } */
/* { dg-final { scan-tree-dump {<(?:widen_)?mult_expr, [^,]*, [^,]*, 9,} "optimized" } } */
/* { dg-final { scan-tree-dump {<(?:widen_)?mult_expr, [^,]*, [^,]*, 22,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 3,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 100,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 715827882,} "optimized" { target int32 } } } */
