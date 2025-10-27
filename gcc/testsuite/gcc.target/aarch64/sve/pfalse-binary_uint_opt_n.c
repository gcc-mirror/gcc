/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_SIGNED_UINT (asr, MXZ)
ALL_INTEGER_UINT (lsl, MXZ)
ALL_UNSIGNED_UINT (lsr, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 32 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 64 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 96 } } */
