/* Test various operators on __fp16 and mixed __fp16/float operands.  */
/* { dg-do compile { target arm*-*-* } } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-mfp16-format=ieee -mfpu=neon-fp16 -mfloat-abi=softfp" } */

#include "arm-fp16-ops.h"

/* We've specified options for hardware float, including fp16 support, so
   we should not see any calls to libfuncs here.  */
/* { dg-final { scan-assembler-not "\tbl\t__.*hf2" } } */
/* { dg-final { scan-assembler-not "\tbl\t__.*hf3" } } */
/* { dg-final { scan-assembler-not "\tbl\t__gnu_h\[a-z\]*_ieee" } } */
/* { dg-final { scan-assembler-not "\tbl\t__gnu_h2f_ieee" } } */
/* { dg-final { scan-assembler-not "\tbl\t__gnu_f2h_ieee" } } */
