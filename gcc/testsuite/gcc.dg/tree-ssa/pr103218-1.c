/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/103218 */

/* These first two are removed during forwprop1 */
signed char f(signed char a)
{
  signed char t = a < 0;
  int tt = (unsigned char)(t << 7);
  return tt;
}
signed char f0(signed char a)
{
  unsigned char t = a < 0;
  int tt = (unsigned char)(t << 7);
  return tt;
}

/* This one is removed during phiopt. */
signed char  f1(signed char a)
{
    if (a < 0)
      return 1u<<7;
    return 0;
}

/* These three examples should remove "a < 0" by optimized. */
/* { dg-final { scan-tree-dump-times "< 0" 0 "optimized"} } */
