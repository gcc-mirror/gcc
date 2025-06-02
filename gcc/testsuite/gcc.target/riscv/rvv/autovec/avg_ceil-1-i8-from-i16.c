/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "avg.h"

#define NT int8_t
#define WT int16_t

DEF_AVG_1(NT, WT, avg_ceil)

/* { dg-final { scan-assembler-times {csrwi\s*vxrm,\s*0} 1 } } */
/* { dg-final { scan-assembler-times {vaadd.vv} 1 } } */
