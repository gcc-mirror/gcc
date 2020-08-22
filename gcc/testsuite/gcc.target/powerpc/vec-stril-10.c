/* { dg-do compile } */
/* { dg-options "-O1 -mdejagnu-cpu=power10" } */
/* See vec-stril-11.c for the same test with -O2 optimization.  */

#include <altivec.h>

vector signed char
doString(vector signed char *vp)
{
  /* Though two built-in functions are called, the implementation
     should use a single instruction to implement both with -O1.  */
  vector signed char result = vec_stril (*vp);
  if (vec_stril_p (*vp))
    return result;
  else
    return doString (vp + 1);
}

/* Enforce that a single dot-form instruction which is properly biased
   for the target's endianness implements this built-in.  */

/* { dg-final { scan-assembler-times {\mvstribl\.} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribl\M[^.]} 0 { target { be } }} } */
/* { dg-final { scan-assembler-times {\mvstribr} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\.} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\M[^.]} 0 { target { le } }} } */
/* { dg-final { scan-assembler-times {\mvstribl} 0 { target { le } } } } */
