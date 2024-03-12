/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_REDUC_PLUS (double, 4)
DEF_REDUC_PLUS (double, 8)
DEF_REDUC_PLUS (double, 16)
DEF_REDUC_PLUS (double, 32)
DEF_REDUC_PLUS (double, 64)
DEF_REDUC_PLUS (double, 128)
DEF_REDUC_PLUS (double, 256)
DEF_REDUC_PLUS (double, 512)

/* { dg-final { scan-assembler-times {vfredusum\.vs} 8 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
