/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

SD_INTEGER_TO_UINT (histcnt, Zv)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 4 } } */
