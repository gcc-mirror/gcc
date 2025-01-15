/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc" { target { rv32 } } } */
/* { dg-options "-march=rv64gc" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-Os" "-Og" "-Oz" "-flto" } } */

// Tests for merging rshifts into sero-extensions.
// s32-casts are skipped as they can be done with one instruction (sext.w).

#include "extend-shift-helpers.h"

// Below "slli (24-N); srai 24" for rv32
// Below "slli ((32+24)-N); srai (32+24)" for rv64
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(1)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(7)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(8)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(9)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(15)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(16)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(17)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(23)
// Below "srai N" for rv32
// Below "sraiw N" for rv64
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(24)
// Below "srai N" for rv32
// Below "slli ((32+24)-N); srai (32+24)" for rv64
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(25)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(31)
// Below compiler warning for rv32
#if __riscv_xlen == 64
// Below "slli ((32+24)-N); srai (32+24)" for rv64
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(32)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(33)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(39)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(40)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(41)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(47)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(48)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(49)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(55)
// Below "srai N" for rv64
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(56)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(57)
SLONG_EXT_SCHAR_RSHIFT_N_SLONG(63)
#endif /* __riscv_xlen == 64 */



// Below "slli (16-N); srai 16" for rv32
// Below "slli ((32+16)-N); srai (32+16)" for rv64
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(1)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(7)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(8)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(9)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(15)
// Below "srai 16" for rv32
// Below "sraiw 16" for rv64
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(16)
// Below "srai N" for rv32
// Below "slli ((32+16)-N); srai (32+16)" for rv64
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(17)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(23)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(24)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(25)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(31)
// Below compiler warning for rv32
#if __riscv_xlen == 64
// Below "slli ((32+16)-N); srai (32+16)" for rv64
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(32)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(33)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(39)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(40)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(41)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(47)
// Below "srai N" for rv64
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(48)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(49)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(55)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(56)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(57)
SLONG_EXT_SSHORT_RSHIFT_N_SLONG(63)
#endif /* __riscv_xlen == 64 */



#if __riscv_xlen == 64
// Below "slli ((32+16)-N); srai (32+16)" for rv64
//    or "slli (16-N); srai 16" for rv64
SINT_EXT_SSHORT_RSHIFT_N_SINT(1)
SINT_EXT_SSHORT_RSHIFT_N_SINT(7)
SINT_EXT_SSHORT_RSHIFT_N_SINT(8)
SINT_EXT_SSHORT_RSHIFT_N_SINT(9)
SINT_EXT_SSHORT_RSHIFT_N_SINT(15)
// Below "srai N" for rv64
SINT_EXT_SSHORT_RSHIFT_N_SINT(16)
SINT_EXT_SSHORT_RSHIFT_N_SINT(17)
SINT_EXT_SSHORT_RSHIFT_N_SINT(23)
SINT_EXT_SSHORT_RSHIFT_N_SINT(24)
SINT_EXT_SSHORT_RSHIFT_N_SINT(25)
// Below "sraiw N" for rv64
SINT_EXT_SSHORT_RSHIFT_N_SINT(31)
#endif /* __riscv_xlen == 64 */



// Below "slli (16-N); srai 16" for rv32
// Below "slli ((32+16)-N); srai (32+16)" for rv64
//    or "slli (16-N); srai 16" for rv64
SINT_EXT_SSHORT_RSHIFT_N_SLONG(9)
SINT_EXT_SSHORT_RSHIFT_N_SLONG(15)



// Below "slli (16-N); srai 16" for rv32
// Below "slli ((32+16)-N); srai (32+16)" for rv64
SLONG_EXT_SSHORT_RSHIFT_N_SINT(9)
SLONG_EXT_SSHORT_RSHIFT_N_SINT(15)

/* { dg-final { scan-assembler-times "slli\t" 17 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "srai\t" 26 { target { rv32 } } } } */

/* { dg-final { scan-assembler-times "slli\t" 44 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "srai\t" 58 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "sraiw\t" 3 { target { rv64 } } } } */
