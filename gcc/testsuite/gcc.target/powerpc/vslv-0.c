/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

__vector unsigned char
doCharShiftLeft (__vector unsigned char *p, __vector unsigned char *q)
{
  __vector unsigned char result, input, shift_distance;
  result = __builtin_vec_vslv (input, shift_distance);
  return result;
}

/* { dg-final { scan-assembler "vslv" } } */
