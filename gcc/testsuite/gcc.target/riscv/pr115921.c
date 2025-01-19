/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-O2 -march=rv64gc_zba" { target { rv64 } } }  */
/* { dg-options "-O2 -march=rv32gc_zba" { target { rv32 } } }  */

typedef unsigned long target_wide_uint_t;

target_wide_uint_t test_ashift_and(target_wide_uint_t x) {
    return (x & 0x3f) << 12;
}

/* { dg-final { scan-assembler-times "\\sandi" 1 } } */
/* { dg-final { scan-assembler-times "\\sslli" 1 } } */

