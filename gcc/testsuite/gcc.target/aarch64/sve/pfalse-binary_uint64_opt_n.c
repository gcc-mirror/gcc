/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

BHS_SIGNED_UINT64 (asr_wide, MXZ)
BHS_INTEGER_UINT64 (lsl_wide, MXZ)
BHS_UNSIGNED_UINT64 (lsr_wide, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 24 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 48 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 72 } } */
