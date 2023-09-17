/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8" } */

#include "def.h"

DEF_REDUC_MAXMIN (int32_t, max, >, 4)
DEF_REDUC_MAXMIN (int32_t, max, >, 8)
DEF_REDUC_MAXMIN (int32_t, max, >, 16)
DEF_REDUC_MAXMIN (int32_t, max, >, 32)
DEF_REDUC_MAXMIN (int32_t, max, >, 64)
DEF_REDUC_MAXMIN (int32_t, max, >, 128)
DEF_REDUC_MAXMIN (int32_t, max, >, 256)
DEF_REDUC_MAXMIN (int32_t, max, >, 512)
DEF_REDUC_MAXMIN (int32_t, max, >, 1024)

DEF_REDUC_MAXMIN (uint32_t, max, >, 4)
DEF_REDUC_MAXMIN (uint32_t, max, >, 8)
DEF_REDUC_MAXMIN (uint32_t, max, >, 16)
DEF_REDUC_MAXMIN (uint32_t, max, >, 32)
DEF_REDUC_MAXMIN (uint32_t, max, >, 64)
DEF_REDUC_MAXMIN (uint32_t, max, >, 128)
DEF_REDUC_MAXMIN (uint32_t, max, >, 256)
DEF_REDUC_MAXMIN (uint32_t, max, >, 512)
DEF_REDUC_MAXMIN (uint32_t, max, >, 1024)

DEF_REDUC_MAXMIN (int32_t, min, <, 4)
DEF_REDUC_MAXMIN (int32_t, min, <, 8)
DEF_REDUC_MAXMIN (int32_t, min, <, 16)
DEF_REDUC_MAXMIN (int32_t, min, <, 32)
DEF_REDUC_MAXMIN (int32_t, min, <, 64)
DEF_REDUC_MAXMIN (int32_t, min, <, 128)
DEF_REDUC_MAXMIN (int32_t, min, <, 256)
DEF_REDUC_MAXMIN (int32_t, min, <, 512)
DEF_REDUC_MAXMIN (int32_t, min, <, 1024)

DEF_REDUC_MAXMIN (uint32_t, min, <, 4)
DEF_REDUC_MAXMIN (uint32_t, min, <, 8)
DEF_REDUC_MAXMIN (uint32_t, min, <, 16)
DEF_REDUC_MAXMIN (uint32_t, min, <, 32)
DEF_REDUC_MAXMIN (uint32_t, min, <, 64)
DEF_REDUC_MAXMIN (uint32_t, min, <, 128)
DEF_REDUC_MAXMIN (uint32_t, min, <, 256)
DEF_REDUC_MAXMIN (uint32_t, min, <, 512)
DEF_REDUC_MAXMIN (uint32_t, min, <, 1024)

/* { dg-final { scan-assembler-times {vredmax\.vs} 9 } } */
/* { dg-final { scan-assembler-times {vredmaxu\.vs} 9 } } */
/* { dg-final { scan-assembler-times {vredmin\.vs} 9 } } */
/* { dg-final { scan-assembler-times {vredminu\.vs} 9 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
