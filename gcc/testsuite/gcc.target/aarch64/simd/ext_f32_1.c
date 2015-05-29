/* Test the `vextf32' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"
#include "ext_f32.x"

/* { dg-final { scan-assembler-times "ext\[ \t\]+\[vV\]\[0-9\]+\.8\[bB\], ?\[vV\]\[0-9\]+\.8\[bB\], ?\[vV\]\[0-9\]+\.8\[bB\], ?#\[0-9\]+\(?:.4)?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
