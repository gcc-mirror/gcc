/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2" } */

#include "../pfalse-unary_0.h"

BOOL (ptest_any, IMPLICITn)
BOOL (ptest_first, IMPLICITn)
BOOL (ptest_last, IMPLICITn)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\tw0, 0\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 3 } } */
