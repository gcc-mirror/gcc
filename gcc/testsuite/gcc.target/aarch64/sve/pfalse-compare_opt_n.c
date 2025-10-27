/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_FLOAT_BOOL (acge, IMPLICIT)
ALL_FLOAT_BOOL (acgt, IMPLICIT)
ALL_FLOAT_BOOL (acle, IMPLICIT)
ALL_FLOAT_BOOL (aclt, IMPLICIT)
ALL_ARITH_BOOL (cmpeq, IMPLICIT)
ALL_ARITH_BOOL (cmpge, IMPLICIT)
ALL_ARITH_BOOL (cmpgt, IMPLICIT)
ALL_ARITH_BOOL (cmple, IMPLICIT)
ALL_ARITH_BOOL (cmplt, IMPLICIT)
ALL_ARITH_BOOL (cmpne, IMPLICIT)
ALL_FLOAT_BOOL (cmpuo, IMPLICIT)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 162 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 162 } } */
