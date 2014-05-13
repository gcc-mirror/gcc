/* Test the `vextu16' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"
#include "ext_u16.x"

/* { dg-final { scan-assembler-times "ext\[ \t\]+\[vV\]\[0-9\]+\.8\[bB\], ?\[vV\]\[0-9\]+\.8\[bB\], ?\[vV\]\[0-9\]+\.8\[bB\], ?#\[0-9\]+\(?:.2\)?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 3 } } */
/* { dg-final { cleanup-saved-temps } } */
