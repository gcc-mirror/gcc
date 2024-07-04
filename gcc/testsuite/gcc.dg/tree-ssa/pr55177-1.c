/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern __INT32_TYPE__ x;

void foo(void)
{
  __INT32_TYPE__ a = __builtin_bswap32(x);
  a &= 0x5a5b5c5d;
  x = __builtin_bswap32(a);
}

/* { dg-final { scan-tree-dump-times "__builtin_bswap32" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "& 1566333786" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "& 1515936861" 0 "optimized"} } */
