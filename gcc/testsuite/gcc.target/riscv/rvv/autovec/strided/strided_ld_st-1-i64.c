/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mno-vector-strict-align -fno-vect-cost-model -fdump-tree-optimized" } */

#include "strided_ld_st.h"

DEF_STRIDED_LD_ST_FORM_1(int64_t)

/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_LOAD " 2 "optimized" { target { any-opts "-O3" } } } } */
/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_STORE " 2 "optimized" { target { any-opts "-O3" } } } } */
/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_LOAD " 1 "optimized" { target { any-opts "-O2" } } } } */
/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_STORE " 1 "optimized" { target { any-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vlse64.v} 1 } } */
/* { dg-final { scan-assembler-times {vsse64.v} 1 { target { any-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsse64.v} 2 { target { any-opts "-O3" } } } } */
