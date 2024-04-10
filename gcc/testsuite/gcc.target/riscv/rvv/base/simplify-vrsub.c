/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

#define VRSUB_WITH_LMUL(LMUL, DTYPE)                        \
  vint##DTYPE##m##LMUL##_t                                  \
  shortcut_for_riscv_vrsub_case_##LMUL##_##DTYPE            \
  (vint##DTYPE##m##LMUL##_t v1,                             \
   size_t vl)                                               \
  {                                                         \
    return __riscv_vrsub_vx_i##DTYPE##m##LMUL (v1, -1, vl); \
  }

VRSUB_WITH_LMUL (1, 16)
VRSUB_WITH_LMUL (1, 32)

/* { dg-final { scan-assembler-times {vnot\.v} 2 } } */
