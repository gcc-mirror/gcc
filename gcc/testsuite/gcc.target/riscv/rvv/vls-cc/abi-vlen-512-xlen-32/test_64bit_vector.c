/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl512b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 512

#include "../common/test_64bit_vector.h"
// Function under test:
//	int32x2_t test_64bit_vector(int32x2_t vec1, int32x2_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ \[ vec1 \]\)[[:space:]]+\(reg.*:V2SI \d+ v8 \[ vec1 \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V2SI \d+ v9 \[ vec2 \]\)\)} "expand" } } */
// Check return value passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ v8\)[[:space:]]+\(reg.*:V2SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
