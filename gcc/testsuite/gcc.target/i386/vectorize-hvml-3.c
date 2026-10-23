/* { dg-do compile { target { { ! ia32 } && glibc } } } */
/* { dg-options "-O2 -ftree-vectorize -funsafe-math-optimizations -fno-math-errno -march=c86-4g-m7 -mno-avx -mveclibabi=hvml" } */

float sinf (float);
double sin (double);

void
sin_f (float *__restrict dst, const float *__restrict src)
{
  for (int i = 0; i < 64; ++i)
    dst[i] = sinf (src[i]);
}

void
sin_d (double *__restrict dst, const double *__restrict src)
{
  for (int i = 0; i < 64; ++i)
    dst[i] = sin (src[i]);
}

/* { dg-final { scan-assembler {\mhvml_vs4_sin\M} } } */
/* { dg-final { scan-assembler {\mhvml_vd2_sin\M} } } */
/* { dg-final { scan-assembler-not {\mhvml_v(s8|s16|d4|d8)_} } } */
