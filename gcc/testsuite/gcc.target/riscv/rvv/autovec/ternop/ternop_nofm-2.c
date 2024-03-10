/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d --param=riscv-autovec-preference=scalable -fdump-tree-optimized-details" } */

#include "ternop-2.c"

/* { dg-final { scan-assembler-times {\tvmacc\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvfmacc\.vv} 9 } } */
/* TODO: we don't have undefine IR for COND_LEN_* operations,
    which will produce redundant move instructions here.
    Will add assembler-not check of 'vmv' instructions in the future.  */
/* { dg-final { scan-tree-dump-times "COND_LEN_FMA" 9 "optimized" } } */
