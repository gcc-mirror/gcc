/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3 -Wno-psabi" } */

#include "overloaded_vget_vset.h"

/* { dg-final { scan-assembler-times {vl[0-9]+re[0-9]+\.v\s+v[0-9]+,\s*0\([ax][0-9]+\)} 14 } } */
/* { dg-final { scan-assembler-times {vs[0-9]+r\.v\s+v[0-9]+,\s*0\([ax][0-9]+\)} 13 } } */
