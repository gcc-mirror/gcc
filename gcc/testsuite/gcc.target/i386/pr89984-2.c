/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx" } */

float
check_f_pos (float x, float y)
{
  return x * __builtin_copysignf (1.0f, y);
}

/* { dg-final { scan-assembler-not "vmovaps" } } */
