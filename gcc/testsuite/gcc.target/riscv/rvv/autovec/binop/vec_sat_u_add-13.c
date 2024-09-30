/* { dg-do compile } */

#include "../vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_FMT_4(uint8_t)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 4 "expand" } } */
/* { dg-final { scan-assembler-times {vsaddu\.vv} 1 } } */
