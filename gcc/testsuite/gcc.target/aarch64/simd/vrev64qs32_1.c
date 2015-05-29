/* Test the `vrev64q_s32' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vrev64qs32.x"

/* { dg-final { scan-assembler-times "rev64\[ \t\]+v\[0-9\]+.4s, ?v\[0-9\]+.4s!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
