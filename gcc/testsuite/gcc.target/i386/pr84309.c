/* PR middle-end/84309 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx -mno-avx2" } */

double pow (double, double) __attribute__((simd));
double exp (double) __attribute__((simd));
extern double a[1024], b[1024];

void
foo (void)
{
  for (int i = 0; i < 1024; ++i)
    a[i] = pow (2.0, b[i]);
}

/* { dg-final { scan-assembler "_ZGVcN4v_exp" } } */
