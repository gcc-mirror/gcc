/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx -msse2" } */

float
check_f_pos (float x, float y)
{
  return x * __builtin_copysignf (1.0f, y);
}
