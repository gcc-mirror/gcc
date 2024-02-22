/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -S -mno-cmpb" } */

float testf (float x, float y)
{
  return __builtin_copysignf (x, y);
}

