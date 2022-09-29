// P1467R9 - Extended floating-point types and standard names.
// { dg-do run { target { c++23 && float16_runtime } } }
// { dg-options "" }
// { dg-add-options float16 }

#ifndef WIDTH
#ifndef __STDCPP_FLOAT16_T__
#error Unexpected
#endif
#define WIDTH 16
#endif

#include <stdarg.h>
#include "ext-floating.h"

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define TYPE CONCAT (_Float, WIDTH)
#define CST(C) CONCAT3 (C, f, WIDTH)
#define CSTU(C) CONCAT3 (C, F, WIDTH)

extern "C" void abort ();

volatile TYPE a = CST (1.0), b = CSTU (2.5), c = -CST (2.5);
volatile TYPE a2 = CST (1.0), z = CST (0.0), nz = -CST (0.0);

// These types are not subject to default argument promotions.

TYPE
vafn (TYPE arg1, ...)
{
  va_list ap;
  TYPE ret;
  va_start (ap, arg1);
  ret = arg1 + va_arg (ap, TYPE);
  va_end (ap);
  return ret;
}

TYPE
fn (TYPE arg)
{
  return arg / 4;
}

int
main (void)
{
  volatile TYPE r;
  r = -b;
  if (r != c)
    abort ();
  r = a + b;
  if (r != CST (3.5))
    abort ();
  r = a - b;
  if (r != -CST (1.5))
    abort ();
  r = 2 * c;
  if (r != -5)
    abort ();
  r = b * c;
  if (r != -CST (6.25))
    abort ();
  r = b / (a + a);
  if (r != CST (1.25))
    abort ();
  r = c * 3;
  if (r != -CST (7.5))
    abort ();
  volatile int i = r;
  if (i != -7)
    abort ();
  r = vafn (a, c);
  if (r != -CST (1.5))
    abort ();
  r = fn (a);
  if (r != CST (0.25))
    abort ();
  if ((a < b) != 1)
    abort ();
  if ((b < a) != 0)
    abort ();
  if ((a < a2) != 0)
    abort ();
  if ((nz < z) != 0)
    abort ();
  if ((a <= b) != 1)
    abort ();
  if ((b <= a) != 0)
    abort ();
  if ((a <= a2) != 1)
    abort ();
  if ((nz <= z) != 1)
    abort ();
  if ((a > b) != 0)
    abort ();
  if ((b > a) != 1)
    abort ();
  if ((a > a2) != 0)
    abort ();
  if ((nz > z) != 0)
    abort ();
  if ((a >= b) != 0)
    abort ();
  if ((b >= a) != 1)
    abort ();
  if ((a >= a2) != 1)
    abort ();
  if ((nz >= z) != 1)
    abort ();
  i = (nz == z);
  if (i != 1)
    abort ();
  i = (a == b);
  if (i != 0)
    abort ();
}
