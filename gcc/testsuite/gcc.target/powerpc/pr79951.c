/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -S -mno-cmpb" } */

float testf (float x, float y)
{
  return __builtin_copysignf (x, y);
}

