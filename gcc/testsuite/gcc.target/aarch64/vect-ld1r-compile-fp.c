/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model" } */

#include "stdint.h"
#include "vect-ld1r.x"

DEF (float)
DEF (double)

/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.2d"} } */

