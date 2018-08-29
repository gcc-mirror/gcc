/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

extern void abort (void);
extern void exit (int);

volatile double x = __builtin_nan ("");
volatile int i;

int
main (void)
{
  i = x == -__builtin_inf ();
  if (i != 0 || fetestexcept (FE_INVALID))
    abort ();
}
