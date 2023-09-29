/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8" } */

#include "def.h"

DEF_REDUC_PLUS (int64_t, 4)
DEF_REDUC_PLUS (int64_t, 8)
DEF_REDUC_PLUS (int64_t, 16)
DEF_REDUC_PLUS (int64_t, 32)
DEF_REDUC_PLUS (int64_t, 64)
DEF_REDUC_PLUS (int64_t, 128)
DEF_REDUC_PLUS (int64_t, 256)
DEF_REDUC_PLUS (int64_t, 512)

DEF_REDUC_PLUS (uint64_t, 4)
DEF_REDUC_PLUS (uint64_t, 8)
DEF_REDUC_PLUS (uint64_t, 16)
DEF_REDUC_PLUS (uint64_t, 32)
DEF_REDUC_PLUS (uint64_t, 64)
DEF_REDUC_PLUS (uint64_t, 128)
DEF_REDUC_PLUS (uint64_t, 256)
DEF_REDUC_PLUS (uint64_t, 512)

/* { dg-final { scan-assembler-times {vredsum\.vs} 16 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
