/* Test various operators on __fp16 and mixed __fp16/float operands.  */
/* { dg-do compile { target arm*-*-* } } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-mfp16-format=ieee -ffast-math -mfpu=neon -mfloat-abi=softfp" } */

#include "arm-fp16-ops.h"

/* We've specified options for hardware float, so we should not see any 
   calls to libfuncs here except for those to the conversion functions.  */
/* { dg-final { scan-assembler-not "\tbl\t__.*hf2" } } */
/* { dg-final { scan-assembler-not "\tbl\t__.*hf3" } } */
/* { dg-final { scan-assembler-not "\tbl\t__gnu_h\[a-z\]*_ieee" } } */
