/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mno-vector-strict-align -fno-vect-cost-model -fdump-tree-optimized" } */

#include "strided_ld_st.h"

DEF_STRIDED_LD_ST_FORM_1(uint16_t)

/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_LOAD " 2 "optimized" { target {
     any-opts "-O3"
     no-opts "-mrvv-vector-bits=zvl"
   } } } } */
/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_STORE " 2 "optimized" { target {
     any-opts "-O3"
     no-opts "-mrvv-vector-bits=zvl"
   } } } } */

/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_LOAD " 1 "optimized" { target {
     any-opts "-O2"
     no-opts "-mrvv-vector-bits=zvl"
   } } } } */
/* { dg-final { scan-tree-dump-times ".MASK_LEN_STRIDED_STORE " 1 "optimized" { target {
     any-opts "-O2"
     no-opts "-mrvv-vector-bits=zvl"
   } } } } */

/* { dg-final { scan-assembler-times {vlse16.v} 1 { target {
     no-opts "-mrvv-vector-bits=zvl"
   } } } } */
/* { dg-final { scan-assembler-times {vsse16.v} 1 { target {
     no-opts "-mrvv-vector-bits=zvl"
   } } } } */
