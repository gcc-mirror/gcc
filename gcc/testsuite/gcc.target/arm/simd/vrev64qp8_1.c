/* Test the `vrev64q_p8' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/vrev64qp8.x"

/* { dg-final { scan-assembler "vrev64\.8\[ \t\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+!?\(\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" } } */
