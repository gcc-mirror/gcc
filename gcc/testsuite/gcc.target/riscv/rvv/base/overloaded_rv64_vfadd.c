/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "overloaded_vfadd.h"

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e16,\s*mf4,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e16,\s*mf4,\s*tu,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e16,\s*mf4,\s*tu,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e16,\s*mf4,\s*ta,\s*mu} 2 } } */
/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 12 } } */
