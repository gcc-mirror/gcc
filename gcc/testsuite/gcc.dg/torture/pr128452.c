/* { dg-do run } */
/* { dg-additional-options "-fassociative-math -fno-signed-zeros -fno-trapping-math" } */

__attribute__((noipa)) double
f (double x, double y, double z)
{
  return (x * y) * __builtin_pow (x, -9223372036854775808.0) + z * x;
}

int main ()
{
  if (f (-1.0, 1.0, 0.0) != -1.0)
    __builtin_abort ();
  if (f (-1.0, 1.0, 1.0) != -2.0)
    __builtin_abort ();
  return 0;
}
