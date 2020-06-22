/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

/* Vector string isolate left-justified on array of unsigned short.  */
vector unsigned short
silj (vector unsigned short arg)
{
  return vec_stril (arg);
}

/* Enforce that a single dot-form instruction which is properly biased
   for the target's endianness implements this built-in.  */

/* { dg-final { scan-assembler-times {\mvstrihl\M} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr\M} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl} 0 { target { le } } } } */
