/* { dg-do compile } */
/* { dg-options "-mabi=lp64s -mfpu=none -march=loongarch64 -O2" } */
/* { dg-final { scan-assembler-not "frecip\\.d" } } */
/* { dg-final { scan-assembler-not "movgr2fr\\.d" } } */
/* { dg-final { scan-assembler-not "movfr2gr\\.d" } } */

/* FPU cannot be used at all.  */

#include "flt-abi-isa-1.c"
