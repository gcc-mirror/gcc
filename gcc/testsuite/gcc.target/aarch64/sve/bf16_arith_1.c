/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-options "-O3 --save-temps" } */

#pragma GCC target "+sve2+sve-b16b16"
#include "bf16_arith_1.h"

/* { dg-final { scan-assembler-times {\tbfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 9 } } */
/* { dg-final { scan-assembler-times {\tbfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tbfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 5 } } */
