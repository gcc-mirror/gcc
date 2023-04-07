/* { dg-do compile } */
/* { dg-options "-mabi=lp64s -mfpu=64 -march=loongarch64 -O2" } */
/* { dg-final { scan-assembler "frecip\\.d" } } */
/* { dg-final { scan-assembler "movgr2fr\\.d" } } */
/* { dg-final { scan-assembler "movfr2gr\\.d" } } */

/* FPU is used for calculation but FPR cannot be used for arguments and
   return values.  */

#include "flt-abi-isa-1.c"
