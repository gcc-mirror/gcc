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

/* No expansion (unknown alignment) */
#define MY_MEM_CMP_N(N)						\
int my_mem_cmp_##N(const char *b1, const char *b2)		\
{								\
  return __builtin_memcmp (b1, b2, N);				\
}

/* No expansion (unknown alignment) */
#define MY_MEM_CMP_CONST_N(N)					\
int my_mem_cmp_const_##N(const char *b1)			\
{								\
  return __builtin_memcmp (b1, myconst##N, sizeof(myconst##N));	\
}

MY_MEM_CMP_N(15)
MY_MEM_CMP_CONST_N(15)

MY_MEM_CMP_N(23)
MY_MEM_CMP_CONST_N(23)

MY_MEM_CMP_N(31)
MY_MEM_CMP_CONST_N(31)

/* { dg-final { scan-assembler-times "\t(call|tail)\tmemcmp" 6 } } */
