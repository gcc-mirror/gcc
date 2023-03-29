/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions_double } */

#include <fenv.h>

extern void abort (void);
extern void exit (int);

double __attribute__ ((noinline, noclone))
foo (double x)
{
  if (__builtin_islessequal (x, 0.0) || __builtin_isgreater (x, 1.0))
    return x + x;
  return x * x;
}

int
main (void)
{
  volatile double x = foo (__builtin_nan (""));
  if (fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
