/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mlsx" } */
/* { dg-require-effective-target loongarch_sx_hw } */

#include "lsx-xorsign.c"

extern void abort ();

#define N 16
float a[N] = {-0.1f, -3.2f, -6.3f, -9.4f,
              -12.5f, -15.6f, -18.7f, -21.8f,
              24.9f, 27.1f, 30.2f, 33.3f,
              36.4f, 39.5f, 42.6f, 45.7f};
float b[N] = {-1.2f, 3.4f, -5.6f, 7.8f,
              -9.0f, 1.0f, -2.0f, 3.0f,
              -4.0f, -5.0f, 6.0f, 7.0f,
              -8.0f, -9.0f, 10.0f, 11.0f};
float r[N];

double ad[N] = {-0.1d,  -3.2d,  -6.3d,  -9.4d,
                -12.5d, -15.6d, -18.7d, -21.8d,
                 24.9d,  27.1d,  30.2d,  33.3d,
                 36.4d,  39.5d,  42.6d, 45.7d};
double bd[N] = {-1.2d,  3.4d, -5.6d,  7.8d,
                -9.0d,  1.0d, -2.0d,  3.0d,
                -4.0d, -5.0d,  6.0d,  7.0d,
                -8.0d, -9.0d, 10.0d, 11.0d};
double rd[N];

void
__attribute__ ((optimize ("-O0")))
check_xorsignf (void)
{
  for (int i = 0; i < N; i++)
    if (r[i] != a[i] * __builtin_copysignf (1.0f, b[i]))
      abort ();
}

void
__attribute__ ((optimize ("-O0")))
check_xorsign (void)
{
  for (int i = 0; i < N; i++)
    if (rd[i] != ad[i] * __builtin_copysign (1.0d, bd[i]))
      abort ();
}

int
main (void)
{
  my_xorsignf (r, a, b, N);
  /* check results:  */
  check_xorsignf ();

  my_xorsign (rd, ad, bd, N);
  /* check results:  */
  check_xorsign ();

  return 0;
}
