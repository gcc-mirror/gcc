/* Test the `vrev32_u16' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vrev32u16.x"

/* { dg-final { scan-assembler-times "rev32\[ \t\]+v\[0-9\]+.4h, ?v\[0-9\]+.4h!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
