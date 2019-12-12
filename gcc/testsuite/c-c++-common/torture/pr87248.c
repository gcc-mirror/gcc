/* PR middle-end/87248 */
/* { dg-do run } */

void
foo (signed char *p, int q)
{
  *p = q & (-__SCHAR_MAX__ - 1) ? (-__SCHAR_MAX__ - 1) : 0;
}

int
bar (long long x)
{
  return x & (-__INT_MAX__ - 1) ? (-__INT_MAX__ - 1) : 0;
}

int
main ()
{
#if __INT_MAX__ > 4 * __SCHAR_MAX__
  signed char a[4];
  foo (a, __SCHAR_MAX__ + 1U);
  foo (a + 1, 2 * (__SCHAR_MAX__ + 1U));
  foo (a + 2, -__INT_MAX__ - 1);
  foo (a + 3, (__SCHAR_MAX__ + 1U) / 2);
  if (a[0] != (-__SCHAR_MAX__ - 1) || a[1] != a[0] || a[2] != a[0] || a[3] != 0)
    __builtin_abort ();
#endif
#if __LONG_LONG_MAX__ > 4 * __INT_MAX__
  if (bar (__INT_MAX__ + 1LL) != (-__INT_MAX__ - 1)
      || bar (2 * (__INT_MAX__ + 1LL)) != (-__INT_MAX__ - 1)
      || bar (-__LONG_LONG_MAX__ - 1) != (-__INT_MAX__ - 1)
      || bar ((__INT_MAX__ + 1LL) / 2) != 0)
    __builtin_abort ();
#endif
  return 0;
}
