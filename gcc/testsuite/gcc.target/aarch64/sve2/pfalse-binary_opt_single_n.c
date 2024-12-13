/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -march=armv8.2-a+sve2+faminmax" } */

#include "../pfalse-binary_0.h"

ALL_FLOAT (amax, MXZ)
ALL_FLOAT (amin, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 24 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 36 } } */
