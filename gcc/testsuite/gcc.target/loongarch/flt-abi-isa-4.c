/* { dg-do compile } */
/* { dg-options "-msoft-float -march=loongarch64 -O2" } */
/* { dg-final { scan-assembler-not "frecip\\.d" } } */
/* { dg-final { scan-assembler-not "movgr2fr\\.d" } } */
/* { dg-final { scan-assembler-not "movfr2gr\\.d" } } */

/* -msoft-float implies both -mabi=lp64s and -mfpu=none.
   FPU cannot be used at all.  */

#include "flt-abi-isa-1.c"
