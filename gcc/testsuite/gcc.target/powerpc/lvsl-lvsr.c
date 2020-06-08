/* Test expected code generation for lvsl and lvsr on little endian.
   Note that lvsl and lvsr are each produced once, but the filename
   causes them to appear twice in the file.  */

/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-O0 -Wno-deprecated" } */
/* { dg-final { scan-assembler-times "lvsl" 2 } } */
/* { dg-final { scan-assembler-times "lvsr" 2 } } */
/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 2 } } */


#include <altivec.h>

float f[20];

void foo ()
{
  vector unsigned char a = vec_lvsl (4, f);
  vector unsigned char b = vec_lvsr (8, f);
}
