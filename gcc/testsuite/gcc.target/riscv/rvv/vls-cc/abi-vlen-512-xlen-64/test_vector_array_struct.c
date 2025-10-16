/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 512

#include "../common/test_vector_array_struct.h"
// Function under test:
//	struct_vector_array_t test_vector_array_struct(struct_vector_array_t s)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v8 \[ s \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v9 \[ s\+16 \]\)\)} "expand" } } */
