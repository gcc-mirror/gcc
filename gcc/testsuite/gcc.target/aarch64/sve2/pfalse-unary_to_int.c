/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-unary_0.h"

ALL_FLOAT_INT (logb, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 3} } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 6 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 9 } } */
