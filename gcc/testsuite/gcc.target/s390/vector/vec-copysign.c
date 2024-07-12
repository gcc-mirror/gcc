/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -ftree-vectorize -mzarch -fno-unroll-loops" } */
/* { dg-final { scan-assembler-times {vgm\t%v[0-9]+,[0-9]+,[0-9]+,3} 1 } } */
/* { dg-final { scan-assembler-times {vgm\t%v[0-9]+,[0-9]+,[0-9]+,2} 1 } } */
/* { dg-final { scan-assembler-times {vsel} 2 } } */

#include <math.h>

#define N 20

double a[N] = {-0.1, -3.2, -6.3, -9.4, -12.5, -15.6, -18.7, -21.8, 24.9,
    27.1, 30.2, 33.3, 36.4, 39.5, 42.6, nan ("123"), __DBL_MIN__ / 2.0,
    -nan ("1"), __DBL_MAX__ * 2.0, -__DBL_MAX__ * 1e199};
double b[N] = {-1.2, 3.4, -5.6, 7.8, -9.0, 1.0, -2.0, 3.0, -4.0, -5.0, 6.0,
    7.0, -8.0, -9.0, 10.0, -11.0, -1., 0., -0., 1.3};
double r[N];
float af[N] = {-0.1, -3.2, -6.3, -9.4, -12.5, -15.6, -18.7, -21.8, 24.9,
    27.1, 30.2, 33.3, 36.4, 39.5, 42.6, nan ("123"), __DBL_MIN__ / 2.0,
    -nan ("1"), __DBL_MAX__ * 2.0, -__DBL_MAX__ * 1e199};
float bf[N] = {-1.2, 3.4, -5.6, 7.8, -9.0, 1.0, -2.0, 3.0, -4.0, -5.0, 6.0,
    7.0, -8.0, -9.0, 10.0, -11.0, -1., 0., -0., 1.3};
float rf[N];

__attribute__ ((__target__ ("arch=z13")))
void
foo (void)
{
  for (int i = 0; i < N; i++)
    r[i] = copysign (a[i], b[i]);
}

__attribute__ ((__target__ ("arch=z14")))
void
foof (void)
{
  for (int i = 0; i < N; i++)
    rf[i] = copysignf (af[i], bf[i]);
}
