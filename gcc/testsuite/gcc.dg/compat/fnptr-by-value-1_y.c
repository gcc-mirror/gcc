#include <stdarg.h>

#include "compat-common.h"

typedef void (*fpi)(int);
typedef void (*fpd)(double);

extern int f1_val;
extern void checki (int, int);

void
test1a (fpi f)
{
  (*f)(1);
}

void
test1b (fpi f, int i)
{
  (*f)(i);
}

void
test1c (double x, fpd f)
{
  (*f)(x);
}

void
test2a (fpi f1, fpd f2)
{
  (*f1)(10);
  (*f2)(10.0);
}

void
test2b (fpi f1, fpd f2, int i)
{
  (*f1)(i);
  (*f2)((double)i);
}

void
test2c (fpi f1, int i, fpd f2)
{
  (*f1)(i);
  (*f2)((double)i);
}

void
test2d (int i, fpi f1, fpd f2)
{
  (*f1)(i);
  (*f2)((double)i);
}

void
test2e (fpi f1, fpd f2, int i, double x)
{
  (*f1)(i);
  (*f2)(x);
}

void
test2f (fpi f1, int i, fpd f2, double x)
{
  (*f1)(i);
  (*f2)(x);
}

void
test2g (fpi f1, int i, double x, fpd f2)
{
  (*f1)(i);
  (*f2)(x);
}

void
test2h (double x, fpd f1, fpi f2, int i)
{
  (*f1)(x);
  (*f2)(i);
}

void
test2i (double x, fpd f1, int i, fpi f2)
{
  (*f1)(x);
  (*f2)(i);
}

void
test2j (int i, double x, fpi f1, fpd f2)
{
  (*f1)(i);
  (*f2)(x);
}

void
testva (int n, ...)
{
  int i;
  va_list ap;
  va_start (ap, n);
  for (i = 0; i < n; i++)
    {
      fpi fp = va_arg (ap, fpi);
      (*fp)(i);
      checki (f1_val, i);
    }
}
