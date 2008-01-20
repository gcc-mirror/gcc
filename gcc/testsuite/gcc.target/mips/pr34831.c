/* { dg-mips-options "-ffast-math -mips64 -mgp32" } */

double
foo (void)
{
  return __builtin_pow (0.0, -1.5);
}
