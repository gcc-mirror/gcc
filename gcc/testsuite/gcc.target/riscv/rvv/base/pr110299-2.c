/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvfh -mabi=ilp32d -O3" } */

#include "pr110299-1.h"
#include "pr110299-2.h"

/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
