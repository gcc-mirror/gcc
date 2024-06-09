/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32d -O3" } */

#include "pr110265-1.h"
#include "pr110265-2.h"

/* { dg-final { scan-assembler-times {vredand\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredmax\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vredmaxu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredmin\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vredminu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredor\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredsum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredxor\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
