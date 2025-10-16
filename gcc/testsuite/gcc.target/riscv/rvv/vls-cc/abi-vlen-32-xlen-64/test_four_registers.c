/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl32b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 32

#include "../common/test_four_registers.h"
// Function under test:
//	int32x16_t test_four_registers(int32x16_t vec1, int32x16_t vec2)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+.*\).*\(reg.*:DI \d+ a1\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ \[ \.result_ptr \]\).*\(reg.*:DI \d+ a0 \[ \.result_ptr \]\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0\).*\(reg.*:DI \d+ \[ \.result_ptr \]\)\)} "expand" } } */
