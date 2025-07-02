/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 --param=aarch64-autovec-preference=sve-only -fno-trapping-math" } */

#include <stdint.h>

/* Ensure that we still emit NOR here, rather than two NOTs.  */

#define TEST_FCM_NOR(TYPE0, TYPE1, CMP, COUNT)		\
  void							\
  f_##TYPE0##_##TYPE1##_##CMP (TYPE0 *__restrict out,	\
			       TYPE1 *__restrict a,	\
			       TYPE1 *__restrict b,	\
			       TYPE1 *__restrict c)	\
  {								    \
    for (unsigned int i = 0; i < COUNT; i++)			    \
      out[i] = !(CMP (a[i], c[i]) | CMP (b[i], c[i])) ? 3 : out[i]; \
  }

#define GT(A, B) ((A) > (B))

TEST_FCM_NOR (uint64_t, float, GT, 32)
TEST_FCM_NOR (uint64_t, _Float16, GT, 32)
TEST_FCM_NOR (uint32_t, _Float16, GT, 64)

TEST_FCM_NOR (uint64_t, float, __builtin_isunordered, 32)
TEST_FCM_NOR (uint64_t, _Float16, __builtin_isunordered, 32)
TEST_FCM_NOR (uint32_t, _Float16, __builtin_isunordered, 64)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 6 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 6 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 6 } } */

/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-not {\tnot\t} } } */
/* { dg-final { scan-assembler-times {\tnor\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b\n} 6 } } */
