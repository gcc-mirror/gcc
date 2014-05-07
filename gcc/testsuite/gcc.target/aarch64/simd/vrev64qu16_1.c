/* Test the `vrev64q_u16' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vrev64qu16.x"

/* { dg-final { scan-assembler-times "rev64\[ \t\]+v\[0-9\]+.8h, ?v\[0-9\]+.8h!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
