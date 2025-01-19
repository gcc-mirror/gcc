/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_FMT_1(uint16_t)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" { target { no-opts
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m1"
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m2"
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m3"
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m4"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m1"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m2"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m3"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m4"
   } } } } */
/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" { target any-opts
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m1"
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m2"
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m3"
     "-O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m4"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m1"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m2"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m3"
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m4"
   } } } } */
/* { dg-final { scan-assembler-times {vsaddu\.vv} 1 } } */
