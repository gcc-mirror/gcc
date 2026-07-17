/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "mma-builtin-2.h"

/* { dg-final { scan-assembler-times {\mxxmfacc\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mxvf64ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gernn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gernn\M} 1 } } */
