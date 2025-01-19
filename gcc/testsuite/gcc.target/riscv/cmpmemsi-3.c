/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

#include <stddef.h>
#define aligned32 __attribute__ ((aligned (32)))

const char myconst15[] aligned32 = { 1, 2, 3, 4, 5, 6, 7,
				     0, 1, 2, 3, 4, 5, 6, 7 };
const char myconst23[] aligned32 = { 1, 2, 3, 4, 5, 6, 7,
				     0, 1, 2, 3, 4, 5, 6, 7,
				     0, 1, 2, 3, 4, 5, 6, 7 };
const char myconst31[] aligned32 = { 1, 2, 3, 4, 5, 6, 7,
				     0, 1, 2, 3, 4, 5, 6, 7,
				     0, 1, 2, 3, 4, 5, 6, 7,
				     0, 1, 2, 3, 4, 5, 6, 7 };

#define MY_MEM_CMP_ALIGNED_N(N)					\
int my_mem_cmp_aligned_##N(const char *b1, const char *b2)	\
{								\
  b1 = __builtin_assume_aligned (b1, 4096);			\
  b2 = __builtin_assume_aligned (b2, 4096);			\
  return __builtin_memcmp (b1, b2, N);				\
}

#define MY_MEM_CMP_ALIGNED_CONST_N(N)				\
int my_mem_cmp_aligned_const_##N(const char *b1)		\
{								\
  b1 = __builtin_assume_aligned (b1, 4096);			\
  return __builtin_memcmp (b1, myconst##N, sizeof(myconst##N));	\
}

MY_MEM_CMP_ALIGNED_N(15)
MY_MEM_CMP_ALIGNED_CONST_N(15)

MY_MEM_CMP_ALIGNED_N(23)
MY_MEM_CMP_ALIGNED_CONST_N(23)

MY_MEM_CMP_ALIGNED_N(31)
MY_MEM_CMP_ALIGNED_CONST_N(31)

/* { dg-final { scan-assembler-not "\t(call|tail)\tmemcmp" } } */
