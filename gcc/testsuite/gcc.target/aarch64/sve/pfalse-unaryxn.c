/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-unary_0.h"

ALL_FLOAT (rinta, MXZ)
ALL_FLOAT (rintm, MXZ)
ALL_FLOAT (rintn, MXZ)
ALL_FLOAT (rintp, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 12} } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 24 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 36 } } */
