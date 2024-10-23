/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include "strided_ld_st.h"
#include "strided_ld_st_data.h"

#define T uint16_t

DEF_STRIDED_LD_ST_FORM_1_WRAP(T)

#define DATA TEST_STRIDED_LD_ST_DATA_WRAP(T)
#define RUN_STRIDED_LD_ST(out, in, stride, size) \
  RUN_STRIDED_LD_ST_FORM_1_WRAP(T, out, in, stride, size)

#include "strided_ld_st_run.h"
