/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl512b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

// Test: Medium vector with ABI_VLEN=32

#include "vls-cc-common.h"

int32x4_t __attribute__((riscv_vls_cc(32))) test_32_abi_vlen_medium_vector(int32x4_t vec) {
    // 128-bit vector with ABI_VLEN=32 -> uses 4 registers
    return vec;
}
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ vec \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ v8\)[[:space:]]+\(reg.*:V4SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
