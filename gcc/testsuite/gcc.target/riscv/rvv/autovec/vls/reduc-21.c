/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8" } */

#include "def.h"

DEF_REDUC_PLUS (float, 4)
DEF_REDUC_PLUS (float, 8)
DEF_REDUC_PLUS (float, 16)
DEF_REDUC_PLUS (float, 32)
DEF_REDUC_PLUS (float, 64)
DEF_REDUC_PLUS (float, 128)
DEF_REDUC_PLUS (float, 256)
DEF_REDUC_PLUS (float, 512)

/* { dg-final { scan-assembler-times {vfredosum\.vs} 8 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
