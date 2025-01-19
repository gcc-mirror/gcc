/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc" { target { rv32 } } } */
/* { dg-options "-march=rv64gc" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

#include <stddef.h>

/* No expansion (unknown size) */
void my_mem_cpy_n(char *b1, const char *b2, size_t n)
{
  __builtin_memcpy (b1, b2, n);
}

/* No expansion (unknown size) */
void my_mem_cpy_aligned(char *b1, const char *b2, size_t n)
{
  b1 = __builtin_assume_aligned (b1, 4096);
  b2 = __builtin_assume_aligned (b2, 4096);
  __builtin_memcpy (b1, b2, n);
}

/* { dg-final { scan-assembler-times "\t(call|tail)\tmemcpy" 2 } } */
