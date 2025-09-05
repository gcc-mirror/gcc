/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "--param=fpr2vr-cost=0" } */

#include "vf_binop.h"
#include "vf_binop_data.h"

#define T    float
#define NAME rsub

DEF_VF_BINOP_REVERSE_CASE_0_WRAP (T, -, NAME)

#define TEST_DATA                        TEST_BINOP_DATA_WRAP(T, NAME)
#define TEST_RUN(T, NAME, out, in, f, n) RUN_VF_BINOP_REVERSE_CASE_0_WRAP(T, NAME, out, in, f, n)

#include "vf_binop_run.h"
