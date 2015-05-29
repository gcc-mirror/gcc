/* Test the `vzipq_u32' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vzipqu32.x"

/* { dg-final { scan-assembler-times "zip1\[ \t\]+v\[0-9\]+\.4s, ?v\[0-9\]+\.4s, ?v\[0-9\]+\.4s!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { scan-assembler-times "zip2\[ \t\]+v\[0-9\]+\.4s, ?v\[0-9\]+\.4s, ?v\[0-9\]+\.4s!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
