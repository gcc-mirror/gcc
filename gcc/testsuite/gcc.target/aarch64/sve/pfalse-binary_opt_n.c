/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

ALL_ARITH (abd, MXZ)
ALL_ARITH (add, MXZ)
ALL_INTEGER (and, MXZ)
B (and, Zv)
ALL_INTEGER (bic, MXZ)
B (bic, Zv)
ALL_FLOAT_AND_SD_INTEGER (div, MXZ)
ALL_FLOAT_AND_SD_INTEGER (divr, MXZ)
ALL_INTEGER (eor, MXZ)
B (eor, Zv)
ALL_ARITH (mul, MXZ)
ALL_INTEGER (mulh, MXZ)
ALL_FLOAT (mulx, MXZ)
B (nand, Zv)
B (nor, Zv)
B (orn, Zv)
ALL_INTEGER (orr, MXZ)
B (orr, Zv)
ALL_ARITH (sub, MXZ)
ALL_ARITH (subr, MXZ)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 224 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 448 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 7 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 679 } } */
