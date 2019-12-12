/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z10" } */

/* According to IEEE 754 2008 4.3 Conversion operations between
   different radixes must use the rounding mode of the target radix.
   On S/390 this means passing the right value in GPR0 to PFPO
   instruction.  */

#include <fenv.h>

double __attribute__((noclone,noinline))
convert (_Decimal64 in)
{
  return (double)in;
}

int
main ()
{
  fesetround (FE_UPWARD);

  if (convert (1e-325DD) != __DBL_DENORM_MIN__)
    __builtin_abort ();

  fesetround (FE_DOWNWARD);

  if (convert (-1e-325DD) != -__DBL_DENORM_MIN__)
    __builtin_abort ();
}
