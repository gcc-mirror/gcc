/* Test the `vuzpq_p16' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vuzpqp16.x"

/* { dg-final { scan-assembler-times "uzp1\[ \t\]+v\[0-9\]+\.8h, ?v\[0-9\]+\.8h, ?v\[0-9\]+\.8h!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { scan-assembler-times "uzp2\[ \t\]+v\[0-9\]+\.8h, ?v\[0-9\]+\.8h, ?v\[0-9\]+\.8h!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
