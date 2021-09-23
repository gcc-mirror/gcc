/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */
/* { dg-options "-Ofast" } */

#pragma GCC target "+nosve"

#include <arm_neon.h>

void s_mult_i (int32_t* restrict res, int32_t* restrict a, int32_t b)
{
    for (int x = 0; x < 16; x++)
      res[x] = a[x] * b;
}

void s_mult_f (float32_t* restrict res, float32_t* restrict a, float32_t b)
{
    for (int x = 0; x < 16; x++)
      res[x] = a[x] * b;
}

/* { dg-final { scan-assembler-times {\s+mul\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.s\[0\]} 4 } } */
/* { dg-final { scan-assembler-times {\s+fmul\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.s\[0\]} 4 } } */
