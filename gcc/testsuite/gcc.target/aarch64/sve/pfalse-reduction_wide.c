/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-unary_0.h"

ALL_ARITH_SCALAR_WIDE (addv, IMPLICIT)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[dwvx]0(?:\.(2s|4h))?, #?0\n\tret\n} 11 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 11 } } */
