/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "overloaded_vloxseg2ei16.h"

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e64,\s*m4,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e64,\s*m4,\s*tu,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e64,\s*m4,\s*tu,\s*mu} 1 } } */
/* { dg-final { scan-assembler-times {vloxseg2ei16\.v\s+v[0-9]+,\s*\([ax][0-9]+\),\s*v[0-9]+} 6 } } */
