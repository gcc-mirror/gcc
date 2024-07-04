/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32d -O3" } */

#include "pr110299-3.h"
#include "pr110299-4.h"

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
