/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl32b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 32

#include "../common/test_equivalent_struct.h"
// Function under test:
//	equivalent_struct_t test_equivalent_struct(equivalent_struct_t s)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ s \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ s \]\)\)} "expand" } } */
// Check return value passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ v8\)[[:space:]]+\(reg.*:V4SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
