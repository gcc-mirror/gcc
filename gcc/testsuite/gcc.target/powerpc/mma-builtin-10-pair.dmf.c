/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

#include "mma-builtin-10-pair.h"

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 2 } } */
