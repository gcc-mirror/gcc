/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl512b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

// Test: Large vector with ABI_VLEN=128

#include "vls-cc-common.h"

int32x8_t __attribute__((riscv_vls_cc(128))) test_128_abi_vlen_large_vector(int32x8_t vec) {
    // 256-bit vector with ABI_VLEN=128 -> uses 2 registers
    return vec;
}
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V8SI \d+ v8 \[ vec \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ v8\)[[:space:]]+\(reg.*:V8SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
