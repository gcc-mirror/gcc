/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mfpmath=both -mavx -fno-math-errno" } */

double foo (double a, double b)
{
  double z = __builtin_fmod (a, 3.14);
  return z * b;
}

/* { dg-final { scan-assembler-not "vmulsd\[ \t]\+%xmm\[0-9]\+, %xmm\[0-9]\+, %xmm\[0-9]\+"} } */
