/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "avg.h"

#define NT int64_t
#define WT int128_t

DEF_AVG_1_WRAP(NT, WT, avg_ceil)

/* { dg-final { scan-assembler-times {csrwi\s*vxrm,\s*0} 1 } } */
/* { dg-final { scan-assembler-times {vaadd.vv} 1 } } */
