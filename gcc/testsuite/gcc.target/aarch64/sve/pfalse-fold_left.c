/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_FLOAT_SCALAR (adda, IMPLICITv)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 3 } } */
