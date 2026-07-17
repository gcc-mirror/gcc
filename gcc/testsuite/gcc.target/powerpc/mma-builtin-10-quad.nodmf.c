/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "mma-builtin-10-quad.h"

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 4 } } */
