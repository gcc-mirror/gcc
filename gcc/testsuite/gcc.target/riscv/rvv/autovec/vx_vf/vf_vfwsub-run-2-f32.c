/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "--param=fpr2vr-cost=0" } */

#include "vf_binop.h"

#define T1    float
#define T2    double
#define NAME sub
#define OP -

DEF_VF_BINOP_WIDEN_CASE_2_WRAP (T1, T2, OP, NAME)

#define TEST_RUN(T1, T2, NAME, out, in, f, n) RUN_VF_BINOP_WIDEN_CASE_2_WRAP(T1, T2, NAME, out, in, f, n)
#define LIMIT -2147483648
#define SINGLE

#include "vf_binop_widen_run.h"
