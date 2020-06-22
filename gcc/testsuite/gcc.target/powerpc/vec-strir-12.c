/* { dg-do compile } */
/* { dg-options "-O1 -mdejagnu-cpu=power10" } */
/* See vec-strir-13.c for the same test with -O2 optimization.  */

#include <altivec.h>

vector unsigned short
doString(vector unsigned short *vp)
{
  /* Though two built-in functions are called, the implementation
     should use a single instruction to implement both with -O1.  */
  vector unsigned short result = vec_strir (*vp);
  if (vec_strir_p (*vp))
    return result;
  else
    return doString (vp + 1);
}

/* Enforce that a single dot-form instruction which is properly biased
   for the target's endianness implements this built-in.  */

/* { dg-final { scan-assembler-times {\mvstrihr\.} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr\M[^.]} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl\.} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihl\M[^.]} 0 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstrihr} 0 { target { le } } } } */

