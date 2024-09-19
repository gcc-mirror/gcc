/* { dg-do run { target aarch64_sve128_hw } } */
/* { dg-options "-O2 -msve-vector-bits=128" } */

#include <arm_sve.h>
#include <stdint.h>

typedef svbool_t pred __attribute__((arm_sve_vector_bits(128)));
typedef svfloat16_t svfloat16_ __attribute__((arm_sve_vector_bits(128)));
typedef svfloat32_t svfloat32_ __attribute__((arm_sve_vector_bits(128)));
typedef svfloat64_t svfloat64_ __attribute__((arm_sve_vector_bits(128)));
typedef svint32_t svint32_ __attribute__((arm_sve_vector_bits(128)));
typedef svint64_t svint64_ __attribute__((arm_sve_vector_bits(128)));
typedef svuint32_t svuint32_ __attribute__((arm_sve_vector_bits(128)));
typedef svuint64_t svuint64_ __attribute__((arm_sve_vector_bits(128)));

#define F(T, TS, P, OP1, OP2)						\
{									\
  T##_t op1 = (T##_t) OP1;						\
  T##_t op2 = (T##_t) OP2;						\
  sv##T##_ res = svmul_##P (pg, svdup_##TS (op1), svdup_##TS (op2));	\
  sv##T##_ exp = svdup_##TS (op1 * op2);				\
  if (svptest_any (pg, svcmpne (pg, exp, res)))				\
    __builtin_abort ();							\
									\
  sv##T##_ res_n = svmul_##P (pg, svdup_##TS (op1), op2);		\
  if (svptest_any (pg, svcmpne (pg, exp, res_n)))			\
    __builtin_abort ();							\
}

#define TEST_TYPES_1(T, TS)						\
  F (T, TS, m, 79, 16)							\
  F (T, TS, z, 79, 16)							\
  F (T, TS, x, 79, 16)

#define TEST_TYPES							\
  TEST_TYPES_1 (float16, f16)						\
  TEST_TYPES_1 (float32, f32)						\
  TEST_TYPES_1 (float64, f64)						\
  TEST_TYPES_1 (int32, s32)						\
  TEST_TYPES_1 (int64, s64)						\
  TEST_TYPES_1 (uint32, u32)						\
  TEST_TYPES_1 (uint64, u64)

#define TEST_VALUES_S_1(B, OP1, OP2)					\
  F (int##B, s##B, x, OP1, OP2)

#define TEST_VALUES_S							\
  TEST_VALUES_S_1 (32, INT32_MIN, INT32_MIN)				\
  TEST_VALUES_S_1 (64, INT64_MIN, INT64_MIN)				\
  TEST_VALUES_S_1 (32, 4, 4)						\
  TEST_VALUES_S_1 (32, -7, 4)						\
  TEST_VALUES_S_1 (32, 4, -7)						\
  TEST_VALUES_S_1 (64, 4, 4)						\
  TEST_VALUES_S_1 (64, -7, 4)						\
  TEST_VALUES_S_1 (64, 4, -7)						\
  TEST_VALUES_S_1 (32, INT32_MAX, (1 << 30))				\
  TEST_VALUES_S_1 (32, (1 << 30), INT32_MAX)				\
  TEST_VALUES_S_1 (64, INT64_MAX, (1ULL << 62))				\
  TEST_VALUES_S_1 (64, (1ULL << 62), INT64_MAX)				\
  TEST_VALUES_S_1 (32, INT32_MIN, (1 << 30))				\
  TEST_VALUES_S_1 (64, INT64_MIN, (1ULL << 62))				\
  TEST_VALUES_S_1 (32, INT32_MAX, 1)					\
  TEST_VALUES_S_1 (32, INT32_MAX, 1)					\
  TEST_VALUES_S_1 (64, 1, INT64_MAX)					\
  TEST_VALUES_S_1 (64, 1, INT64_MAX)					\
  TEST_VALUES_S_1 (32, INT32_MIN, 16)					\
  TEST_VALUES_S_1 (64, INT64_MIN, 16)					\
  TEST_VALUES_S_1 (32, INT32_MAX, -5)					\
  TEST_VALUES_S_1 (64, INT64_MAX, -5)					\
  TEST_VALUES_S_1 (32, INT32_MIN, -4)					\
  TEST_VALUES_S_1 (64, INT64_MIN, -4)

#define TEST_VALUES_U_1(B, OP1, OP2)					\
  F (uint##B, u##B, x, OP1, OP2)

#define TEST_VALUES_U							\
  TEST_VALUES_U_1 (32, UINT32_MAX, UINT32_MAX)				\
  TEST_VALUES_U_1 (64, UINT64_MAX, UINT64_MAX)				\
  TEST_VALUES_U_1 (32, UINT32_MAX, (1 << 31))				\
  TEST_VALUES_U_1 (64, UINT64_MAX, (1ULL << 63))			\
  TEST_VALUES_U_1 (32, 7, 4)						\
  TEST_VALUES_U_1 (32, 4, 7)						\
  TEST_VALUES_U_1 (64, 7, 4)						\
  TEST_VALUES_U_1 (64, 4, 7)						\
  TEST_VALUES_U_1 (32, 7, 3)						\
  TEST_VALUES_U_1 (64, 7, 3)						\
  TEST_VALUES_U_1 (32, 11, 1)						\
  TEST_VALUES_U_1 (64, 11, 1)

#define TEST_VALUES							\
  TEST_VALUES_S								\
  TEST_VALUES_U

int
main (void)
{
  const pred pg = svptrue_b8 ();
  TEST_TYPES
  TEST_VALUES
  return 0;
}
