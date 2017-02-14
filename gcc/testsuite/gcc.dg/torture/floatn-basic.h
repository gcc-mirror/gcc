/* Basic tests for _FloatN / _FloatNx types: compile and execution
   tests for valid code.  Before including this file, define WIDTH as
   the value N; define EXT to 1 for _FloatNx and 0 for _FloatN.  */

#include <stdarg.h>

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
# define CST(C) CONCAT4 (C, f, WIDTH, x)
# define CSTU(C) CONCAT4 (C, F, WIDTH, x)
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define CSTU(C) CONCAT3 (C, F, WIDTH)
#endif

extern void exit (int);
extern void abort (void);

volatile TYPE a = CST (1.0), b = CSTU (2.5), c = -CST (2.5);
volatile TYPE a2 = CST (1.0), z = CST (0.0), nz = -CST (0.0);

/* These types are not subject to default argument promotions.  */

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
krfn (arg)
     TYPE arg;
{
  return arg + 1;
}

TYPE krprofn (TYPE);
TYPE
krprofn (arg)
     TYPE arg;
{
  return arg * 3;
}

TYPE
profn (TYPE arg)
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
  r = krfn (b);
  if (r != CST (3.5))
    abort ();
  r = krprofn (a);
  if (r != CST (3.0))
    abort ();
  r = profn (a);
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
  exit (0);
}
