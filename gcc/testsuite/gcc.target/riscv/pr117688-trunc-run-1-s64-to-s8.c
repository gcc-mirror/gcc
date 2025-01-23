/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "pr117688.h"

DEFINE_SIGNED_SAT_TRUNC_RUN(int64_t, int8_t, INT8_MIN, INT8_MAX)
