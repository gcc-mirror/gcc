/* { dg-options "-O3 --save-temps" } */

#pragma GCC target "arch=armv8-a+sve-b16b16"
#include "bf16_arith_1.h"

/* { dg-final { scan-assembler-not {\tbfadd\t} } } */
/* { dg-final { scan-assembler-not {\tbfsub\t} } } */
/* { dg-final { scan-assembler-not {\tbfmul\t} } } */
