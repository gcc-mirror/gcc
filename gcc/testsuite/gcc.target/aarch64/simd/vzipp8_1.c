/* Test the `vzip_p8' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -fno-inline" } */

#include <arm_neon.h>
#include "vzipp8.x"

/* { dg-final { scan-assembler-times "zip1\[ \t\]+v\[0-9\]+\.8b, ?v\[0-9\]+\.8b, ?v\[0-9\]+\.8b!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { scan-assembler-times "zip2\[ \t\]+v\[0-9\]+\.8b, ?v\[0-9\]+\.8b, ?v\[0-9\]+\.8b!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
