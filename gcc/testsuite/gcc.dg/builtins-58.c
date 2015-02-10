/* { dg-do compile } */
/* { dg-options "-O -ffast-math -std=c99 -fno-ident" } */

#include "builtins-config.h"

#ifdef HAVE_C99_RUNTIME
double test1 (double x)
{
  return __builtin_pow (x, 1./3.);
}

double test2 (double x)
{
  return __builtin_pow (x, 4./3.);
}

double test3a (double x)
{
  return __builtin_pow (x, 5./3.);
}

double test3b (double x)
{
  return __builtin_pow (x, -5./3.);
}

double test4 (double x)
{
  return __builtin_pow (x, 7./3.);
}
#endif

/* { dg-final { scan-assembler-not {pow\M} } } */
