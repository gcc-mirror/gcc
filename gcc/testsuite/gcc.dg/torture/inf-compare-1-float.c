/* { dg-do run { xfail { powerpc*-*-* } } } */
/* remove the xfail for powerpc when pr58684 is fixed */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-skip-if "fenv" { powerpc-ibm-aix* } } */
/* { dg-additional-options "-fno-tree-dominator-opts -fno-tree-vrp" } */

#include <fenv.h>

extern void abort (void);
extern void exit (int);

volatile float x = __builtin_nan ("");
volatile int i;

int
main (void)
{
  i = x > __builtin_inf ();
  if (i != 0 || !fetestexcept (FE_INVALID))
    abort ();
}
