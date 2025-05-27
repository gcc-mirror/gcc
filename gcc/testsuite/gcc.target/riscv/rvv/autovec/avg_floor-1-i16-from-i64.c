/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "avg.h"

#define NT int16_t
#define WT int64_t

DEF_AVG_0(NT, WT, avg_floor)

/* { dg-final { scan-assembler-times {csrwi\s*vxrm,\s*2} 1 } } */
/* { dg-final { scan-assembler-times {vaadd.vv} 1 } } */
