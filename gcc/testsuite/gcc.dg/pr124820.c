/* PR middle-end/124820 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) double
foo (double x, double y, double z)
{
  return -__builtin_fma (x, y, z);
}

__attribute__((noipa)) double
bar (double x, double y, double z)
{
  return -__builtin_fma (x, y, -z);
}

__attribute__((noipa)) double
baz (double x, double y, double z)
{
  return -__builtin_fma (-x, y, z);
}

__attribute__((noipa)) double
qux (double x, double y, double z)
{
  return -__builtin_fma (-x, y, -z);
}

int
main ()
{
  /* x * y + z is exactly zero in each case, so the fma result is +0.0
     and its negation must be -0.0.  */
  if (!__builtin_signbit (foo (1.0, 1.0, -1.0))
      || !__builtin_signbit (foo (0.0, 0.0, -0.0))
      || !__builtin_signbit (bar (2.0, 3.0, 6.0))
      || !__builtin_signbit (baz (1.0, 1.0, 1.0))
      || !__builtin_signbit (qux (1.0, 1.0, -1.0)))
    __builtin_abort ();
  return 0;
}
