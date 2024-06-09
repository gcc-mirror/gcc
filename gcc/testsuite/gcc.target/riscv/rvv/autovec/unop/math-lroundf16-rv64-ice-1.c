/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-mrvv-max-lmul=m4 -march=rv64gcv_zvfh_zfh -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */

#include "test-math.h"

TEST_UNARY_CALL_CVT (_Float16, long, __builtin_lroundf16)
