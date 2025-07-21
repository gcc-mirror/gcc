/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include "../pfalse-binary_0.h"

B (brkn, Zv)
B (brkpa, Zv)
B (brkpb, Zv)
ALL_DATA (splice, IMPLICITv)

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 3 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tmov\tz0\.d, z1\.d\n\tret\n} 12 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 15 } } */
