/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string isolate right-justified on array of unsigned short.  */
int
sirj_p (vector unsigned short arg)
{
  return vec_strir_p (arg);
}

/* Enforce that a single dot-form instruction which is properly biased
   for the target's endianness implements this built-in.  */

/* { dg-final { scan-assembler-times {\mvstrihr\.} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr\M[^.]} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl\.} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl\M[^.]} 0 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr} 0 { target { le } } } } */
