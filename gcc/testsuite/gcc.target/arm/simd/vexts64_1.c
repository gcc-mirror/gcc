/* Test the `vexts64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O3 -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/ext_s64.x"

/* Don't scan assembler for vext - it can be optimized into a move from r0.  */
/* { dg-final { cleanup-saved-temps } } */
