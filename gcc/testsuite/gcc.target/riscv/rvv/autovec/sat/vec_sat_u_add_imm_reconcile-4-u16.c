/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-rtl-expand-details" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_IMM_FMT_3(uint64_t, 18446744073709551615ull)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 4 "expand" } } */
