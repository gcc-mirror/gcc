/* Test the `vrev64_s32' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vrev64s32.x"

/* { dg-final { scan-assembler-times "rev64\[ \t\]+v\[0-9\]+.2s, ?v\[0-9\]+.2s!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
