/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_ARITH (addp, MXv)
ALL_ARITH (maxp, MXv)
ALL_FLOAT (maxnmp, MXv)
ALL_ARITH (minp, MXv)
ALL_FLOAT (minnmp, MXv)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 39 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 39 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 78 } } */
