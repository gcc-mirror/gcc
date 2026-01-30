/* PR tree-optimization/123864 */

[[gnu::noipa]] static int
foo (long long x)
{
  return __builtin_mul_overflow_p (x, ~0U, x);
}

int
main ()
{
  if (foo (0))
    __builtin_abort ();
#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8 && __CHAR_BIT__ == 8
  if (foo (__INT_MAX__ + 1LL))
    __builtin_abort ();
  if (!foo (__INT_MAX__ + 2LL))
    __builtin_abort ();
  if (foo (-__INT_MAX__ - 1LL))
    __builtin_abort ();
  if (!foo (-__INT_MAX__ - 2LL))
    __builtin_abort ();
#endif
}
