/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-binary_0.h"

BH_INTEGER_BOOL (match, IMPLICITv)
BH_INTEGER_BOOL (nmatch, IMPLICITv)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 8 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 8 } } */
