/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8" } */

#include "def.h"

DEF_REDUC_PLUS (int8_t, 4)
DEF_REDUC_PLUS (int8_t, 8)
DEF_REDUC_PLUS (int8_t, 16)
DEF_REDUC_PLUS (int8_t, 32)
DEF_REDUC_PLUS (int8_t, 64)
DEF_REDUC_PLUS (int8_t, 128)
DEF_REDUC_PLUS (int8_t, 256)
DEF_REDUC_PLUS (int8_t, 512)
DEF_REDUC_PLUS (int8_t, 1024)
DEF_REDUC_PLUS (int8_t, 2048)
DEF_REDUC_PLUS (int8_t, 4096)

DEF_REDUC_PLUS (uint8_t, 4)
DEF_REDUC_PLUS (uint8_t, 8)
DEF_REDUC_PLUS (uint8_t, 16)
DEF_REDUC_PLUS (uint8_t, 32)
DEF_REDUC_PLUS (uint8_t, 64)
DEF_REDUC_PLUS (uint8_t, 128)
DEF_REDUC_PLUS (uint8_t, 256)
DEF_REDUC_PLUS (uint8_t, 512)
DEF_REDUC_PLUS (uint8_t, 1024)
DEF_REDUC_PLUS (uint8_t, 2048)
DEF_REDUC_PLUS (uint8_t, 4096)

/* { dg-final { scan-assembler-times {vredsum\.vs} 22 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
