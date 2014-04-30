/* Test the `vuzp_s32' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vuzps32.x"

/* { dg-final { scan-assembler-times "uzp1\[ \t\]+v\[0-9\]+\.2s, ?v\[0-9\]+\.2s, ?v\[0-9\]+\.2s!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { scan-assembler-times "uzp2\[ \t\]+v\[0-9\]+\.2s, ?v\[0-9\]+\.2s, ?v\[0-9\]+\.2s!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
