/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string isolate left-justified on array of signed char.  */
int
silj_p (vector signed char arg)
{
  return vec_stril_p (arg);
}

/* Enforce that a single dot-form instruction which is properly biased
   for the target's endianness implements this built-in.  */

/* { dg-final { scan-assembler-times {\mvstribl\.} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribl\M[^.]} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribr} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\.} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\M[^.]} 0 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribl} 0 { target { le } } } } */
