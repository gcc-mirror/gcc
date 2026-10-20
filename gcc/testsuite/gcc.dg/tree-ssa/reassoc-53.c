/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

/* Undistribution must not try to decrement a non-constant exponent
   of pow or powi.  */

__attribute__((noipa)) double
f (double x, double y, double z, double n)
{
  return (x * y) * __builtin_pow (x, n) + z * x;
}

__attribute__((noipa)) double
g (double x, double y, double z, int n)
{
  return (x * y) * __builtin_powi (x, n) + z * x;
}

int
main (void)
{
  if (f (4.0, 1.0, 0.0, 3.0) != 256.0)
    __builtin_abort ();
  if (g (4.0, 1.0, 0.0, 3) != 256.0)
    __builtin_abort ();
  return 0;
}
