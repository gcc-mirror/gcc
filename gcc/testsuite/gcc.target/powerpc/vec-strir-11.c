/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#include <altivec.h>

vector signed char
doString(vector signed char *vp)
{
  /* Though two built-in functions are called, the implementation
     should use a single instruction to implement both and should
     convert tail recursion to iteration with two copies of the "loop
     body" when compiled with -O2 or -O3.  */
  vector signed char result = vec_strir (*vp);
  if (vec_strir_p (*vp))
    return result;
  else
    return doString (vp + 1);
}

/* Enforce that exactly two dot-form instructions which are properly biased
   for the target's endianness implement this built-in.  */

/* { dg-final { scan-assembler-times {\mvstribr\.} 2 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribr\M[^.]} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribl} 0 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mvstribl\.} 2 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribl\M[^.]} 0 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mvstribr} 0 { target { le } } } } */
