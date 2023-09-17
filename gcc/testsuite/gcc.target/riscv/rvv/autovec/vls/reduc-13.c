/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_REDUC_MAXMIN (float, max, >, 4)
DEF_REDUC_MAXMIN (float, max, >, 8)
DEF_REDUC_MAXMIN (float, max, >, 16)
DEF_REDUC_MAXMIN (float, max, >, 32)
DEF_REDUC_MAXMIN (float, max, >, 64)
DEF_REDUC_MAXMIN (float, max, >, 128)
DEF_REDUC_MAXMIN (float, max, >, 256)
DEF_REDUC_MAXMIN (float, max, >, 512)
DEF_REDUC_MAXMIN (float, max, >, 1024)

DEF_REDUC_MAXMIN (float, min, <, 4)
DEF_REDUC_MAXMIN (float, min, <, 8)
DEF_REDUC_MAXMIN (float, min, <, 16)
DEF_REDUC_MAXMIN (float, min, <, 32)
DEF_REDUC_MAXMIN (float, min, <, 64)
DEF_REDUC_MAXMIN (float, min, <, 128)
DEF_REDUC_MAXMIN (float, min, <, 256)
DEF_REDUC_MAXMIN (float, min, <, 512)
DEF_REDUC_MAXMIN (float, min, <, 1024)

/* { dg-final { scan-assembler-times {vfredmax\.vs} 9 } } */
/* { dg-final { scan-assembler-times {vfredmin\.vs} 9 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
