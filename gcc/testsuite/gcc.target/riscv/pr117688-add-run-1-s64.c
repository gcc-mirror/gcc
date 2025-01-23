/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "pr117688.h"

DEFINE_SIGNED_SAT_ADD_RUN(int64_t, INT64_MIN, INT64_MAX)
