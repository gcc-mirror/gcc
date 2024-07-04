/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -S -mno-cmpb" } */
/* { dg-require-effective-target powerpc_vsx } */

float testf (float x, float y)
{
  return __builtin_copysignf (x, y);
}

