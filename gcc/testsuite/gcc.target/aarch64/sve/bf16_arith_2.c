/* { dg-options "-O3 --save-temps" } */

#pragma GCC target "arch=armv9-a+sve2"
#include "bf16_arith_1.h"

/* { dg-final { scan-assembler-not {\tbfadd\t} } } */
/* { dg-final { scan-assembler-not {\tbfsub\t} } } */
/* { dg-final { scan-assembler-not {\tbfmul\t} } } */
