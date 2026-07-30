/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O2 -fdump-tree-cplxlower1" } */
/* { dg-final { scan-tree-dump-not "\.BSWAP \\\(" "cplxlower1" } } */
/* { dg-final { scan-tree-dump-not "\.BITREVERSE \\\(" "cplxlower1" } } */
/* { dg-final { scan-tree-dump-times "\.PARITY \\\(x_" 2 "cplxlower1" } } */

int
foo (unsigned _BitInt(512) x)
{
  return __builtin_parityg (__builtin_bswapg (x));
}

int
bar (unsigned _BitInt(575) x)
{
  return __builtin_parityg (__builtin_bitreverseg (x));
}
