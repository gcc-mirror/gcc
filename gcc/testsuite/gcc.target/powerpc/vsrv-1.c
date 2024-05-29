/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector unsigned char
doCharShiftLeft (__vector unsigned char *p, __vector unsigned char *q)
{
  __vector unsigned char result, input, shift_distance;
  result = vec_srv (input, shift_distance);
  return result;
}

/* { dg-final { scan-assembler "vsrv" } } */
