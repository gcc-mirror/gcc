/* Test the `vextu64' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"
#include "ext_u64.x"

/* Do not scan-assembler.  An EXT instruction could be emitted, but would merely
   return its first argument, so it is legitimate to optimize it out.  */
/* { dg-final { cleanup-saved-temps } } */
