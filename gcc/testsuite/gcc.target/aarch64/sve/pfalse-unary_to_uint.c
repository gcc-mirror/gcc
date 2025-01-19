/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-unary_0.h"

ALL_SIGNED_UINT (cls, MXZ)
ALL_INTEGER_UINT (clz, MXZ)
ALL_DATA_UINT (cnt, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n}  24 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 48 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 72 } } */
