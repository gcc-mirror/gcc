/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

#include "mma-builtin-10-quad.h"

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
// 2 loads compared to 4 in power, since vsx and dmr regs are independent.
/* { dg-final { scan-assembler-times {\mlxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 4 } } */
