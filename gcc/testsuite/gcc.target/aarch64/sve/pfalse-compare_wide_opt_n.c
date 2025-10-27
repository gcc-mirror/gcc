/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

BHS_SIGNED_WIDE_BOOL (cmpeq_wide, IMPLICIT)
BHS_INTEGER_WIDE_BOOL (cmpge_wide, IMPLICIT)
BHS_INTEGER_WIDE_BOOL (cmpgt_wide, IMPLICIT)
BHS_INTEGER_WIDE_BOOL (cmple_wide, IMPLICIT)
BHS_INTEGER_WIDE_BOOL (cmplt_wide, IMPLICIT)
BHS_SIGNED_WIDE_BOOL (cmpne_wide, IMPLICIT)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 60 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 60 } } */
