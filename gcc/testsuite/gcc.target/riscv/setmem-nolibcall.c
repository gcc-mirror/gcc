/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mtune=generic-ooo" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

#define SET_VAR(N)				\
void set_var_##N (void *dst, int c)		\
{						\
  __builtin_memset (dst, c, N);			\
}

#define SET_CST(N)				\
void set_cst_##N (void *dst)			\
{						\
  __builtin_memset (dst, 3, N);			\
}

#define SET_ZERO(N)				\
void set_zero_##N (void *dst)			\
{						\
  __builtin_memset (dst, 0, N);			\
}

#define FOR_EACH_LEN(M)					\
  M(17) M(18) M(20) M(24) M(25) M(31) M(32) M(33)	\
  M(40) M(63) M(64) M(65) M(96)

FOR_EACH_LEN (SET_VAR)
FOR_EACH_LEN (SET_CST)
FOR_EACH_LEN (SET_ZERO)

/* { dg-final { scan-assembler-not "memset" } } */
/* { dg-final { scan-assembler-not "\tcall\t" } } */
/* { dg-final { scan-assembler-not "\ttail\t" } } */
