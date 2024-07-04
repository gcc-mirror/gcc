/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_REDUC_PLUS(TYPE)			\
TYPE __attribute__ ((noinline, noclone))	\
reduc_plus_##TYPE (TYPE *a, int n)		\
{						\
  TYPE r = 0;					\
  for (int i = 0; i < n; ++i)			\
    r += a[i];					\
  return r;					\
}

#define TEST_PLUS(T)				\
  T (int8_t)					\
  T (int16_t)					\
  T (int32_t)					\
  T (int64_t)					\
  T (uint8_t)					\
  T (uint16_t)					\
  T (uint32_t)					\
  T (uint64_t)					\
  T (_Float16)					\
  T (float)					\
  T (double)

TEST_PLUS (DEF_REDUC_PLUS)

#define DEF_REDUC_MAXMIN(TYPE, NAME, CMP_OP)	\
TYPE __attribute__ ((noinline, noclone))	\
reduc_##NAME##_##TYPE (TYPE *a, int n)		\
{						\
  TYPE r = 13;					\
  for (int i = 0; i < n; ++i)			\
    r = a[i] CMP_OP r ? a[i] : r;		\
  return r;					\
}

#define TEST_MAXMIN(T)				\
  T (int8_t, max, >)				\
  T (int16_t, max, >)				\
  T (int32_t, max, >)				\
  T (int64_t, max, >)				\
  T (uint8_t, max, >)				\
  T (uint16_t, max, >)				\
  T (uint32_t, max, >)				\
  T (uint64_t, max, >)				\
  T (_Float16, max, >)				\
  T (float, max, >)				\
  T (double, max, >)				\
						\
  T (int8_t, min, <)				\
  T (int16_t, min, <)				\
  T (int32_t, min, <)				\
  T (int64_t, min, <)				\
  T (uint8_t, min, <)				\
  T (uint16_t, min, <)				\
  T (uint32_t, min, <)				\
  T (uint64_t, min, <)				\
  T (_Float16, min, <)				\
  T (float, min, <)				\
  T (double, min, <)

TEST_MAXMIN (DEF_REDUC_MAXMIN)

#define DEF_REDUC_BITWISE(TYPE, NAME, BIT_OP)	\
TYPE __attribute__ ((noinline, noclone))	\
reduc_##NAME##_##TYPE (TYPE *a, int n)		\
{						\
  TYPE r = 13;					\
  for (int i = 0; i < n; ++i)			\
    r BIT_OP a[i];				\
  return r;					\
}

#define TEST_BITWISE(T)				\
  T (int8_t, and, &=)				\
  T (int16_t, and, &=)				\
  T (int32_t, and, &=)				\
  T (int64_t, and, &=)				\
  T (uint8_t, and, &=)				\
  T (uint16_t, and, &=)				\
  T (uint32_t, and, &=)				\
  T (uint64_t, and, &=)				\
						\
  T (int8_t, ior, |=)				\
  T (int16_t, ior, |=)				\
  T (int32_t, ior, |=)				\
  T (int64_t, ior, |=)				\
  T (uint8_t, ior, |=)				\
  T (uint16_t, ior, |=)				\
  T (uint32_t, ior, |=)				\
  T (uint64_t, ior, |=)				\
						\
  T (int8_t, xor, ^=)				\
  T (int16_t, xor, ^=)				\
  T (int32_t, xor, ^=)				\
  T (int64_t, xor, ^=)				\
  T (uint8_t, xor, ^=)				\
  T (uint16_t, xor, ^=)				\
  T (uint32_t, xor, ^=)				\
  T (uint64_t, xor, ^=)

TEST_BITWISE (DEF_REDUC_BITWISE)

/* { dg-final { scan-assembler-times {vredsum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {vredmax\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredmaxu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredmin\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredminu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vredand\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {vredor\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {vredxor\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {vfredusum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vfredmax\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vfredmin\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 3 } } */
