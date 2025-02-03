/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "pr117688.h"

DEFINE_SIGNED_SAT_TRUNC_RUN(int32_t, int16_t, INT16_MIN, INT16_MAX)
