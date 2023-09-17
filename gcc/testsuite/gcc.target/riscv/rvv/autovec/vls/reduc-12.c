/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_REDUC_MAXMIN (_Float16, max, >, 4)
DEF_REDUC_MAXMIN (_Float16, max, >, 8)
DEF_REDUC_MAXMIN (_Float16, max, >, 16)
DEF_REDUC_MAXMIN (_Float16, max, >, 32)
DEF_REDUC_MAXMIN (_Float16, max, >, 64)
DEF_REDUC_MAXMIN (_Float16, max, >, 128)
DEF_REDUC_MAXMIN (_Float16, max, >, 256)
DEF_REDUC_MAXMIN (_Float16, max, >, 512)
DEF_REDUC_MAXMIN (_Float16, max, >, 1024)
DEF_REDUC_MAXMIN (_Float16, max, >, 2048)

DEF_REDUC_MAXMIN (_Float16, min, <, 4)
DEF_REDUC_MAXMIN (_Float16, min, <, 8)
DEF_REDUC_MAXMIN (_Float16, min, <, 16)
DEF_REDUC_MAXMIN (_Float16, min, <, 32)
DEF_REDUC_MAXMIN (_Float16, min, <, 64)
DEF_REDUC_MAXMIN (_Float16, min, <, 128)
DEF_REDUC_MAXMIN (_Float16, min, <, 256)
DEF_REDUC_MAXMIN (_Float16, min, <, 512)
DEF_REDUC_MAXMIN (_Float16, min, <, 1024)
DEF_REDUC_MAXMIN (_Float16, min, <, 2048)

/* { dg-final { scan-assembler-times {vfredmax\.vs} 10 } } */
/* { dg-final { scan-assembler-times {vfredmin\.vs} 10 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
