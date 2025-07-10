/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_ARITH (max, MXZ)
ALL_ARITH (min, MXZ)
ALL_FLOAT (maxnm, MXZ)
ALL_FLOAT (minnm, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 56 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 112 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 168 } } */
