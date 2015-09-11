/* Test the `vextQp16' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -O3 -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/extq_p16.x"

/* { dg-final { scan-assembler-times "vext\.16\[ \t\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #\[0-9\]+!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 7 } } */
