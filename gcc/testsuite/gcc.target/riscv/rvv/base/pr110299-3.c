/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32f -mabi=ilp32f -O3" } */

#include "pr110299-3.h"

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
