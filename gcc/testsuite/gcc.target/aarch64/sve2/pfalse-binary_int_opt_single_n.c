/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-binary_0.h"

ALL_INTEGER_INT (rshl, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 16 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 32 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 48 } } */
