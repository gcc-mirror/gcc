/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "mma-builtin-10-pair.h"

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 2 } } */
