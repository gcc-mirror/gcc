/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "--param=fpr2vr-cost=0" } */

#include "vf_mulop.h"
#include "vf_mulop_data.h"

#define T    _Float16
#define NAME add

DEF_VF_MULOP_CASE_0_WRAP (T, +, +, NAME)

#define TEST_DATA                        TEST_MULOP_DATA_WRAP(T, NAME)
#define TEST_RUN(T, NAME, out, in, x, n) RUN_VF_MULOP_CASE_0_WRAP(T, NAME, out, in, x, n)

#include "vf_mulop_run.h"
