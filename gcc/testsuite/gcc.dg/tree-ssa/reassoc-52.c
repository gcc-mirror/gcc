/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

/* Undistribution must not treat pow (x, 2.5) as x * pow (x, 1.5)
   by truncating its exponent.  */

__attribute__((noipa)) double
f (double x, double y, double z)
{
  return (x * y) * __builtin_pow (x, 2.5) + z * x;
}

int
main (void)
{
  if (f (4.0, 1.0, 0.0) != 128.0)
    __builtin_abort ();
  return 0;
}
