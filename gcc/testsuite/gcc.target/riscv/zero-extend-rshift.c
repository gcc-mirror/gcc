/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc" { target { rv32 } } } */
/* { dg-options "-march=rv64gc" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

// Tests for merging rshifts into zero-extensions.
// u8-casts are skipped as they can be done with one instruction (andi 0xff).

#include "extend-shift-helpers.h"

// Below "slli (16-N); srli 16" for rv32
// Below "slli ((32+16)-N); srli (32+16)" for rv64
ULONG_EXT_USHORT_RSHIFT_N_ULONG(1)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(7)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(8)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(9)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(15)
// Below "srli 16" for rv32
// Below "srliw 16" for rv64
ULONG_EXT_USHORT_RSHIFT_N_ULONG(16)
// Below "srli N" for rv32
// Below "slli ((32+16)-N); srli (32+16)" for rv64
ULONG_EXT_USHORT_RSHIFT_N_ULONG(17)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(23)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(24)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(25)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(31)
// Below compiler warning for rv32
#if __riscv_xlen == 64
// Below "slli ((32+16)-N); srli (32+16)" for rv64
ULONG_EXT_USHORT_RSHIFT_N_ULONG(32)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(33)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(39)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(40)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(41)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(47)
// Below "srli N" for rv64
ULONG_EXT_USHORT_RSHIFT_N_ULONG(48)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(49)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(55)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(56)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(57)
ULONG_EXT_USHORT_RSHIFT_N_ULONG(63)
#endif /* __riscv_xlen == 64 */



// Below "srli N" for rv32
// Below "slli (32-N); srli 32" for rv64
ULONG_EXT_UINT_RSHIFT_N_ULONG(1)
ULONG_EXT_UINT_RSHIFT_N_ULONG(7)
ULONG_EXT_UINT_RSHIFT_N_ULONG(8)
ULONG_EXT_UINT_RSHIFT_N_ULONG(9)
ULONG_EXT_UINT_RSHIFT_N_ULONG(15)
ULONG_EXT_UINT_RSHIFT_N_ULONG(16)
ULONG_EXT_UINT_RSHIFT_N_ULONG(17)
ULONG_EXT_UINT_RSHIFT_N_ULONG(23)
ULONG_EXT_UINT_RSHIFT_N_ULONG(24)
ULONG_EXT_UINT_RSHIFT_N_ULONG(25)
ULONG_EXT_UINT_RSHIFT_N_ULONG(31)
// Below compiler warning for rv32
#if __riscv_xlen == 64
// Below "srli N" for rv64
ULONG_EXT_UINT_RSHIFT_N_ULONG(32)
ULONG_EXT_UINT_RSHIFT_N_ULONG(33)
ULONG_EXT_UINT_RSHIFT_N_ULONG(39)
ULONG_EXT_UINT_RSHIFT_N_ULONG(40)
ULONG_EXT_UINT_RSHIFT_N_ULONG(41)
ULONG_EXT_UINT_RSHIFT_N_ULONG(47)
ULONG_EXT_UINT_RSHIFT_N_ULONG(48)
ULONG_EXT_UINT_RSHIFT_N_ULONG(49)
ULONG_EXT_UINT_RSHIFT_N_ULONG(55)
ULONG_EXT_UINT_RSHIFT_N_ULONG(56)
ULONG_EXT_UINT_RSHIFT_N_ULONG(57)
ULONG_EXT_UINT_RSHIFT_N_ULONG(63)
#endif /* __riscv_xlen == 64 */



#if __riscv_xlen == 64
// Below "slli ((32+16)-N); srli (32+16)" for rv64
UINT_EXT_USHORT_RSHIFT_N_UINT(1)
UINT_EXT_USHORT_RSHIFT_N_UINT(7)
UINT_EXT_USHORT_RSHIFT_N_UINT(8)
UINT_EXT_USHORT_RSHIFT_N_UINT(9)
UINT_EXT_USHORT_RSHIFT_N_UINT(15)
// Below "srliw N" for rv64
UINT_EXT_USHORT_RSHIFT_N_UINT(16)
UINT_EXT_USHORT_RSHIFT_N_UINT(17)
UINT_EXT_USHORT_RSHIFT_N_UINT(23)
UINT_EXT_USHORT_RSHIFT_N_UINT(24)
UINT_EXT_USHORT_RSHIFT_N_UINT(25)
UINT_EXT_USHORT_RSHIFT_N_UINT(31)
#endif /* __riscv_xlen == 64 */



// Below "slli (16-N); srli 16" for rv32
// Below "slli ((32+16)-N); srli (32+16)" for rv64
UINT_EXT_USHORT_RSHIFT_N_ULONG(9)
UINT_EXT_USHORT_RSHIFT_N_ULONG(15)



// Below "slli (16-N); srli 16" for rv32
// Below "slli ((32+16)-N); srli (32+16)" for rv64
ULONG_EXT_USHORT_RSHIFT_N_UINT(9)
ULONG_EXT_USHORT_RSHIFT_N_UINT(15)

/* { dg-final { scan-assembler-times "slli\t" 9 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "srli\t" 26 { target { rv32 } } } } */

/* { dg-final { scan-assembler-times "slli\t" 36 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "srli\t" 54 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "srliw\t" 7 { target { rv64 } } } } */
