/* Test the `vuzp_u8' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vuzpu8.x"

/* { dg-final { scan-assembler-times "uzp1\[ \t\]+v\[0-9\]+\.8b, ?v\[0-9\]+\.8b, ?v\[0-9\]+\.8b!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { scan-assembler-times "uzp2\[ \t\]+v\[0-9\]+\.8b, ?v\[0-9\]+\.8b, ?v\[0-9\]+\.8b!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
