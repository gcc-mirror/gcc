/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh -mabi=ilp32 -O3" } */

#include "overloaded_vget_vset.h"

/* { dg-final { scan-assembler-times {vmv1r.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
