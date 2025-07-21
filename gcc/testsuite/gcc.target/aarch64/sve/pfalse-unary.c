/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-unary_0.h"

ALL_FLOAT_AND_SIGNED (abs, MXZ)
B (brka, MZ)
B (brkb, MZ)
ALL_INTEGER (cnot, MXZ)
HSD_INTEGER (extb, MXZ)
SD_INTEGER (exth, MXZ)
D_INTEGER (extw, MXZ)
B (mov, Z)
ALL_FLOAT_AND_SIGNED (neg, MXZ)
ALL_INTEGER (not, MXZ)
B (not, Z)
B (pfirst, IMPLICIT)
ALL_INTEGER (rbit, MXZ)
ALL_FLOAT (recpx, MXZ)
HSD_INTEGER (revb, MXZ)
SD_INTEGER (revh, MXZ)
D_INTEGER (revw, MXZ)
ALL_FLOAT (rinti, MXZ)
ALL_FLOAT (rintx, MXZ)
ALL_FLOAT (rintz, MXZ)
ALL_FLOAT (sqrt, MXZ)
SD_DATA (compact, IMPLICIT)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tret\n} 80 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmovi?\t[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0\n\tret\n} 160 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 4 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 244 } } */
