/* { dg-do compile } */

#include "../vec_sat_arith.h"

DEF_VEC_SAT_U_TRUNC_FMT_2 (uint32_t, uint64_t)

/* { dg-final { scan-rtl-dump-times ".SAT_TRUNC " 2 "expand" } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 1 } } */
