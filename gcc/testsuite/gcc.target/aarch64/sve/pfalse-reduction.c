/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -fdump-tree-optimized -funwind-tables" } */

#include "../pfalse-unary_0.h"

ALL_INTEGER_SCALAR (andv, IMPLICIT)
ALL_INTEGER_SCALAR (eorv, IMPLICIT)
ALL_ARITH_SCALAR (maxv, IMPLICIT)
ALL_ARITH_SCALAR (minv, IMPLICIT)
ALL_INTEGER_SCALAR (orv, IMPLICIT)
ALL_FLOAT_SCALAR (maxnmv, IMPLICIT)
ALL_FLOAT_SCALAR (minnmv, IMPLICIT)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\t[wx]0, 0\n\tret\n} 20 } } */
/* { dg-final { scan-tree-dump-times "return  Nan" 6 "optimized" } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\t[wx]0, -1\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\t[wxz]0(?:\.[sd])?, #?-?[1-9]+[0-9]*\n\tret\n} 27 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\t(movi|mvni)\tv0\.(2s|4h), 0x[0-9a-f]+, [a-z]+ [0-9]+\n\tret\n} 5 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 52 } } */

/* The sum of tested cases is 52 + 12, because mov [wx]0, -1 is tested in two
   patterns.  */
