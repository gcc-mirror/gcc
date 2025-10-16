/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve64d_zvl64b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_register_exhaustion_mixed.h"
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v1 \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ v1 \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ v2 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v3 \]\)[[:space:]]+\(reg.*:V4SI \d+ v12 \[ v3 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v4 \]\)[[:space:]]+\(reg.*:V4SI \d+ v14 \[ v4 \]\)\)} "expand" } } */
// Check argument 5 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v5 \]\)[[:space:]]+\(reg.*:V4SI \d+ v16 \[ v5 \]\)\)} "expand" } } */
// Check argument 6 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v6 \]\)[[:space:]]+\(reg.*:V4SI \d+ v18 \[ v6 \]\)\)} "expand" } } */
// Check argument 7 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v7 \]\)[[:space:]]+\(reg.*:V4SI \d+ v20 \[ v7 \]\)\)} "expand" } } */
// Check vector argument 8 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v8 \]\)[[:space:]]+\(reg.*:V4SI \d+ v22 \[ v8 \]\)\)} "expand" } } */
