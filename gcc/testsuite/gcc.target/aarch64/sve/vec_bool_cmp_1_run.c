/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O3 -fno-inline" } */

#include "vec_bool_cmp_1.c"

#define N 103

#define TEST_VEC_BOOL(NAME, OP, VARTYPE, INDUCTYPE)		\
{								\
  INDUCTYPE i;							\
  VARTYPE src[N];						\
  VARTYPE dst[N];						\
  for (i = 0; i < N; i++)					\
    {								\
      src[i] = i;						\
      dst[i] = i * 2;						\
      asm volatile ("" ::: "memory");				\
    }								\
  vec_bool_##NAME##_##VARTYPE##_##INDUCTYPE (dst, src, 13,	\
					     97, 0xFF);		\
  for (i = 0; i < 13; i++)					\
    if (dst[i] != (VARTYPE) (0 OP 1 ? i : i * 2))		\
      __builtin_abort ();					\
  for (i = 13; i < 97; i++)					\
    if (dst[i] != (VARTYPE) (1 OP (i != 0x3D) ? i : i * 2))	\
      __builtin_abort ();					\
  for (i = 97; i < N; i++)					\
    if (dst[i] != (i * 2))					\
      __builtin_abort ();					\
}

int __attribute__ ((optimize (1)))
main ()
{
  TEST_ALL (TEST_VEC_BOOL)
  return 0;
}
