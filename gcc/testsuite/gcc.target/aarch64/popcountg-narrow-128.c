/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

typedef unsigned __int128 u128;

/* Narrowed to unsigned int: x fits in 32 bits.  */
int f1 (u128 x)
{
  if (x > 0xFFFFFFFFu)
    __builtin_unreachable ();
  return __builtin_popcountg (x);
}

/* Narrowed to unsigned long long: x fits in 64 bits but not 32.  */
int f2 (u128 x)
{
  if (x > 0xFFFFFFFFFFFFFFFFull)
    __builtin_unreachable ();
  return __builtin_popcountg (x);
}

/* Not narrowed: no range information.  */
int f3 (u128 x)
{
  return __builtin_popcountg (x);
}

/* Not narrowed: x <= 2^64 needs 65 bits.  */
int f4 (u128 x)
{
  if (x > ((u128) 1 << 64))
    __builtin_unreachable ();
  return __builtin_popcountg (x);
}

/* f1 and f2 are narrowed.  */
/* { dg-final { scan-tree-dump-times {\.POPCOUNT \(_[0-9]+\)} 2 "optimized" } } */

/* f3 and f4 keep the 128-bit argument.  */
/* { dg-final { scan-tree-dump-times {\.POPCOUNT \(x_} 2 "optimized" } } */
