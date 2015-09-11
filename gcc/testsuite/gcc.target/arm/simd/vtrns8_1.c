/* Test the `vtrns8' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -O1 -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/vtrns8.x"

/* { dg-final { scan-assembler-times "vtrn\.8\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
