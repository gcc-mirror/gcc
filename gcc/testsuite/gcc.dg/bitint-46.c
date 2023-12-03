/* PR middle-end/112807 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=gnu23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 256
__attribute__((noipa)) int
foo (_BitInt (256) a, _BitInt (2) b)
{
  if (a < 0 || a > ~0U)
    return -1;
  return __builtin_sub_overflow_p (a, b, 0);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  if (foo (-5wb, 1wb) != -1
      || foo (1 + (_BitInt (256)) ~0U, -2) != -1
      || foo (0, 0) != 0
      || foo (0, 1) != 0
      || foo (0, -1) != 0
      || foo (~0U, 0) != 1
      || foo (__INT_MAX__, 0) != 0
      || foo (__INT_MAX__, -1) != 1
      || foo (1 + (_BitInt (256)) __INT_MAX__, 0) != 1
      || foo (1 + (_BitInt (256)) __INT_MAX__, 1) != 0
      || foo (1 + (_BitInt (256)) __INT_MAX__, -2) != 1)
    __builtin_abort ();
#endif
}
