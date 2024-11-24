/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

#include <stddef.h>

/* No expansion (unknown size) */
int my_mem_cmp_n(const char *b1, const char *b2, size_t n)
{
  return __builtin_memcmp (b1, b2, n);
}

/* No expansion (unknown size) */
int my_mem_cmp_aligned(const char *b1, const char *b2, size_t n)
{
  b1 = __builtin_assume_aligned (b1, 4096);
  b2 = __builtin_assume_aligned (b2, 4096);
  return __builtin_memcmp (b1, b2, n);
}

/* { dg-final { scan-assembler-times "\t(call|tail)\tmemcmp" 2 } } */
