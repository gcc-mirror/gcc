/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include "vsub-template.h"

/* { dg-final { scan-assembler-times {\tvsub\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvrsub\.vi} 16 } } */

/* { dg-final { scan-assembler-times {\tvfsub\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvfsub\.vf} 3 } } */
/* { dg-final { scan-assembler-times {\tvfrsub\.vf} 6 } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_SUB" 12 "optimized" } } */

/* Do not expect vfrsub for now, because we do not properly
   handle vop.vx and vfop.vf yet.  */
/* { dg-final { scan-assembler-times {\tvfrsub\.vv} 0 } } */
