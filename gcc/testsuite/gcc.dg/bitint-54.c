/* PR tree-optimization/113003 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 131
int
foo (_BitInt(7) x)
{
  return __builtin_mul_overflow_p (x, 1046555807606105294475452482332716433408wb, 0);
}

#ifdef __SIZEOF_INT128__
int
bar (unsigned __int128 x)
{
  return __builtin_sub_overflow_p (340282366920938463463374607431768211457uwb, x, 0);
}
#endif
#else
int i;
#endif
