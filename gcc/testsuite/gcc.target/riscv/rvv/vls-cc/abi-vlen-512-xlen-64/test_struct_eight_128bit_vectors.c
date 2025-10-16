/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 512

#include "../common/test_struct_eight_128bit_vectors.h"
// Function under test:
//	eight_128bit_vectors_struct_t test_struct_eight_128bit_vectors(eight_128bit_vectors_struct_t s)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v8 \[ s \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v9 \[ s\+16 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v10 \[ s\+32 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v11 \[ s\+48 \]\)\)} "expand" } } */
// Check argument 5 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v12 \[ s\+64 \]\)\)} "expand" } } */
// Check argument 6 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v13 \[ s\+80 \]\)\)} "expand" } } */
// Check argument 7 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v14 \[ s\+96 \]\)\)} "expand" } } */
// Check argument 8 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+\)[[:space:]]+\(reg:V4SI \d+ v15 \[ s\+112 \]\)\)} "expand" } } */
