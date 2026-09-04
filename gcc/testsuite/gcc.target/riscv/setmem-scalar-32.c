/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -mtune=generic-ooo -mstringop-strategy=scalar" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" "-flto" } } */

#define SET_VAR(N)				\
void set_var_##N (void *dst, int c)		\
{						\
  dst = __builtin_assume_aligned (dst, 4);	\
  __builtin_memset (dst, c, N);			\
}

#define SET_CST(N)				\
void set_cst_##N (void *dst)			\
{						\
  dst = __builtin_assume_aligned (dst, 4);	\
  __builtin_memset (dst, 3, N);			\
}

SET_VAR(16)
SET_VAR(32)
SET_VAR(48)

SET_CST(16)
SET_CST(32)
SET_CST(48)

/* { dg-final { scan-assembler-times "sw\t" 48 } } */
/* { dg-final { scan-assembler-not "sb\t" } } */
/* { dg-final { scan-assembler-not "memset" } } */
