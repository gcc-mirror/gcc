/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl512b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

// Test: Very large vector with ABI_VLEN=256

#include "vls-cc-common.h"

int32x16_t __attribute__((riscv_vls_cc(256))) test_256_abi_vlen_very_large_vector(int32x16_t vec) {
    // 512-bit vector with ABI_VLEN=256 -> uses 2 registers
    return vec;
}
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V16SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V16SI \d+ v8 \[ vec \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V16SI \d+ v8\)[[:space:]]+\(reg.*:V16SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
