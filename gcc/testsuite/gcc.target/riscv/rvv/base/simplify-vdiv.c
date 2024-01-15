/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

#define VDIV_WITH_LMUL(LMUL, DTYPE)                        \
  vint##DTYPE##m##LMUL##_t                                  \
  shortcut_for_riscv_vdiv_case_##LMUL##_##DTYPE            \
  (vint##DTYPE##m##LMUL##_t v1,                             \
   size_t vl)                                               \
  {                                                         \
    return __riscv_vdiv_vx_i##DTYPE##m##LMUL (v1, -1, vl); \
  }

VDIV_WITH_LMUL (1, 16)
VDIV_WITH_LMUL (1, 32)

/* { dg-final { scan-assembler-times {vneg\.v} 2 } } */
