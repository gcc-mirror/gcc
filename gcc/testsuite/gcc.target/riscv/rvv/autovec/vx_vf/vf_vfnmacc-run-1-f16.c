/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-march=rv64gcv_zvfh --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"
#include "vf_mulop_data.h"

#define T    _Float16
#define NAME nadd

DEF_VF_MULOP_ACC_CASE_0_WRAP (T, +, -, NAME)

#define TEST_DATA                        TEST_MULOP_DATA_WRAP(T, NAME)
#define TEST_RUN(T, NAME, c, b, x, n) RUN_VF_MULOP_ACC_CASE_0_WRAP(T, NAME, b, c, x, n)
#define TEST_OUT b

#include "vf_mulop_run.h"
