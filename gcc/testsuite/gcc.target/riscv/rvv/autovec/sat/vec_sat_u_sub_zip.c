/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -fdump-rtl-expand-details" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_SUB_ZIP_WRAP(uint16_t, uint32_t)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 6 "expand" { target { any-opts
     "-mrvv-vector-bits=scalable"
   } } } } */
/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 4 "expand" { target { any-opts
     "-mrvv-vector-bits=zvl"
   } } } } */
/* { dg-final { scan-assembler-times {vssubu\.vv} 3 { target { any-ops
     "-mrvv-vector-bits=scalable"
   } } } } */
/* { dg-final { scan-assembler-times {vssubu\.vv} 2 { target { any-ops
     "-mrvv-vector-bits=zvl"
   } } } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 2 } } */
