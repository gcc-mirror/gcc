/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl512b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

// Test: Small vector with ABI_VLEN=32

#include "vls-cc-common.h"

int32x2_t __attribute__((riscv_vls_cc(32))) test_32_abi_vlen_small_vector(int32x2_t vec) {
    // 64-bit vector with ABI_VLEN=32 -> uses 2 registers
    return vec;
}
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V2SI \d+ v8 \[ vec \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ v8\)[[:space:]]+\(reg.*:V2SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
