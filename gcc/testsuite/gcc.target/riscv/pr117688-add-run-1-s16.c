/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "pr117688.h"

DEFINE_SIGNED_SAT_ADD_RUN(int16_t, INT16_MIN, INT16_MAX)
