/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_INTEGER (hadd, MXZ)
ALL_INTEGER (hsub, MXZ)
ALL_INTEGER (hsubr, MXZ)
ALL_INTEGER (qadd, MXZ)
ALL_INTEGER (qsub, MXZ)
ALL_INTEGER (qsubr, MXZ)
ALL_INTEGER (rhadd, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 112 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 224 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 336 } } */
