/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=gpr2vr-cost=0" } */

#include "vx_widen.h"
#include "vx_widen_data.h"

#define WT        uint64_t
#define NT        uint32_t
#define NAME      add
#define TEST_DATA DEF_BINARY_WIDEN_STRUCT_0_VAR_WRAP(WT, NT, NAME)
#define DATA_TYPE DEF_BINARY_WIDEN_STRUCT_0_TYPE_WRAP(WT, NT, NAME)

DEF_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, +, NAME)

#define TEST_RUN(WT, NT, NAME, vd, vs2, rs1, N) \
  RUN_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, NAME, vd, vs2, rs1, N)

#include "vx_widen_vx_run.h"
