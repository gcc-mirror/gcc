/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8" } */

#include "def.h"

DEF_REDUC_PLUS (_Float16, 4)
DEF_REDUC_PLUS (_Float16, 8)
DEF_REDUC_PLUS (_Float16, 16)
DEF_REDUC_PLUS (_Float16, 32)
DEF_REDUC_PLUS (_Float16, 64)
DEF_REDUC_PLUS (_Float16, 128)
DEF_REDUC_PLUS (_Float16, 256)
DEF_REDUC_PLUS (_Float16, 512)
DEF_REDUC_PLUS (_Float16, 1024)
DEF_REDUC_PLUS (_Float16, 2048)

/* { dg-final { scan-assembler-times {vfredosum\.vs} 10 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
