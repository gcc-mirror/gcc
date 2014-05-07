/* Test the `vrev32q_s8' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vrev32qs8.x"

/* { dg-final { scan-assembler-times "rev32\[ \t\]+v\[0-9\]+.16b, ?v\[0-9\]+.16b!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
