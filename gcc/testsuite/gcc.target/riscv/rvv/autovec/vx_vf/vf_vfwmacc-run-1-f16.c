/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-march=rv64gcv_zvfh --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

#define T1    _Float16
#define T2    float
#define NAME acc
#define OP +
#define NEG +

DEF_VF_MULOP_WIDEN_CASE_0_WRAP (T1, T2, OP, NEG, NAME)

#define TEST_RUN(T1, T2, NAME, out, in, f, n) RUN_VF_MULOP_WIDEN_CASE_0_WRAP(T1, T2, NAME, out, in, f, n)
#define LIMIT -32768

#include "vf_mulop_widen_run.h"
