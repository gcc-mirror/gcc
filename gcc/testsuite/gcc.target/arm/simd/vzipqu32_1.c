/* Test the `vzipQu32' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -O1 -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/vzipqu32.x"

/* { dg-final { scan-assembler-times "vzip\.32\[ \t\]+\[qQ\]\[0-9\]+, ?\[qQ\]\[0-9\]+!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
