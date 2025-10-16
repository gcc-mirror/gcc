/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve64d_zvl64b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_mixed_args.h"
// Function under test:
//	int test_mixed_args(int scalar1, int32x4_t vec1, float scalar2, int32x4_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ vec1 \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ vec1 \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ vec2 \]\)\)} "expand" } } */
