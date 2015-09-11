/* Test the `vrev16q_u8' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/vrev16qu8.x"

/* { dg-final { scan-assembler "vrev16\.8\[ \t\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+!?\(\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" } } */
