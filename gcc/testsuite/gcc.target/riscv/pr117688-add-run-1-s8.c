/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "pr117688.h"

DEFINE_SIGNED_SAT_ADD_RUN(int8_t, INT8_MIN, INT8_MAX)
