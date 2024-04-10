/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "overloaded_vreinterpret.h"

/* { dg-final { scan-assembler-times {vmv1r.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
