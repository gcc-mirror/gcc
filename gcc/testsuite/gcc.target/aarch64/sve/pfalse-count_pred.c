/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-unary_0.h"

ALL_PRED_UINT64 (cntp, IMPLICIT)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\tx0, 0\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 4 } } */
