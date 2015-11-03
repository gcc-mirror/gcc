/* { dg-do link } */
/* { dg-options "-std=c99 -ffinite-math-only -O" } */

#include "builtins-config.h"

extern void link_error (void);

extern double floor (double);
extern double trunc (double);
extern double fabs (double);

void test (double x)
{
#ifdef HAVE_C99_RUNTIME
  if (floor (fabs (x)) != trunc (fabs (x)))
    link_error ();
#endif
  if (__builtin_lfloor (fabs (x)) != (long)fabs (x))
    link_error ();
}

int main (void)
{
  return 0;
}
