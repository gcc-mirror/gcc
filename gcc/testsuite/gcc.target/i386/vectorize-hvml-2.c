/* { dg-do compile { target { { ! ia32 } && glibc } } } */
/* { dg-options "-O2 -ftree-vectorize -fno-unsafe-math-optimizations -fno-math-errno -march=c86-4g-m7 -mprefer-vector-width=512 -mveclibabi=hvml" } */

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

/* { dg-final { scan-assembler-not {\mhvml_v} } } */
