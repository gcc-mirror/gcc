/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-binary_0.h"

ALL_DATA (clasta, IMPLICITv)
ALL_DATA (clastb, IMPLICITv)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 24 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 24 } } */
