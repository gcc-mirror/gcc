/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

__vector unsigned char
doCharShiftLeft (__vector unsigned char *p, __vector unsigned char *q)
{
  __vector unsigned char result, input, shift_distance;
  result = vec_slv (input, shift_distance);
  return result;
}

/* { dg-final { scan-assembler "vslv" } } */
