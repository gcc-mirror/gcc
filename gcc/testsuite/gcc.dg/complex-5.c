/* PR middle-end/33088 */
/* Origin: Joseph S. Myers <jsm28@gcc.gnu.org> */

/* { dg-do run { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } */
/* { dg-options "-std=c99 -O -ffloat-store -lm" } */

#include <fenv.h>
#include <stdlib.h>

volatile int x[1024];

void __attribute__((noinline))
fill_stack (void)
{
  volatile int y[1024];
  int i;
  for (i = 0; i < 1024; i++)
    y[i] = 0x7ff00000;
  for (i = 0; i < 1024; i++)
    x[i] = y[i];
}

volatile _Complex double vc;

void __attribute__((noinline))
use_complex (_Complex double c)
{
  vc = c;
}

double t0, t1, t2, t3;

#define USE_COMPLEX(X, R, C) \
  do { __real__ X = R; __imag__ X = C; use_complex (X); } while (0)

void __attribute__((noinline))
use_stack (void)
{
  _Complex double a, b, c, d;
  USE_COMPLEX (a, t0, t1);
  USE_COMPLEX (b, t1, t2);
  USE_COMPLEX (c, t2, t3);
  USE_COMPLEX (d, t3, t0);
}

int
main (void)
{
  fill_stack ();
  feclearexcept (FE_INVALID);
  use_stack ();
  if (fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
