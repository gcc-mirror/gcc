/* Test the `vexts32' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -O3 -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/ext_s32.x"

/* { dg-final { scan-assembler-times "vext\.32\[ \t\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #\[0-9\]+!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
