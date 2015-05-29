/* Test the `vrev32u16' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include "../../aarch64/simd/vrev32u16.x"

/* { dg-final { scan-assembler "vrev32\.16\[ \t\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+!?\(\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" } } */
