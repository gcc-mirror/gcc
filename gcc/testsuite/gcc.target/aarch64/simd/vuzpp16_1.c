/* Test the `vuzp_p16' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vuzpp16.x"

/* { dg-final { scan-assembler-times "uzp1\[ \t\]+v\[0-9\]+\.4h, ?v\[0-9\]+\.4h, ?v\[0-9\]+\.4h!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { scan-assembler-times "uzp2\[ \t\]+v\[0-9\]+\.4h, ?v\[0-9\]+\.4h, ?v\[0-9\]+\.4h!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
