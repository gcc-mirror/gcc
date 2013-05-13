
/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

#include "vaddv-intrinsic.x"

/* { dg-final { scan-assembler "faddp\\ts\[0-9\]+"} } */
/* { dg-final { scan-assembler-times "faddp\\tv\[0-9\]+\.4s" 2} } */
/* { dg-final { scan-assembler "faddp\\td\[0-9\]+"} } */
