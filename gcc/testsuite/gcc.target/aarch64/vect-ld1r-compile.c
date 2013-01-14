/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model" } */

#include "stdint.h"
#include "vect-ld1r.x"

DEF (int8_t)
DEF (int16_t)
DEF (int32_t)
DEF (int64_t)

/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.8b"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.16b"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.4h"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.8h"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "ldr\\t\x\[0-9\]+"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.2d"} } */
