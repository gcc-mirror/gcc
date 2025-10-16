/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve64d_zvl64b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_fp_vs_int_vectors.h"
// Function under test:
//	float test_fp_vs_int_vectors(int32x4_t int_vec, float32x4_t float_vec,
//	                             double64x2_t double_vec)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ int_vec \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ int_vec \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SF \d+ \[ float_vec \]\)[[:space:]]+\(reg.*:V4SF \d+ v10 \[ float_vec \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2DF \d+ \[ double_vec \]\)[[:space:]]+\(reg.*:V2DF \d+ v12 \[ double_vec \]\)\)} "expand" } } */
