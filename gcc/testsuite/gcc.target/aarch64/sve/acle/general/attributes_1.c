/* { dg-options "-msve-vector-bits=256" } */

#include <arm_sve.h>

#ifndef __ARM_FEATURE_SVE_BITS
#error "__ARM_FEATURE_SVE_BITS is not defined but should be"
#endif

#if __ARM_FEATURE_SVE_VECTOR_OPERATORS != 1
#error "__ARM_FEATURE_SVE_VECTOR_OPERATORS should be equal to 1"
#endif

#ifndef __cplusplus
#define alignof _Alignof
#endif

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))
#define GNU_ATTR __attribute__ ((vector_size (N / 8)))

typedef svint8_t fixed_int8_t FIXED_ATTR;
typedef svint16_t fixed_int16_t FIXED_ATTR;
typedef svint32_t fixed_int32_t FIXED_ATTR;
typedef svint64_t fixed_int64_t FIXED_ATTR;

typedef svuint8_t fixed_uint8_t FIXED_ATTR;
typedef svuint16_t fixed_uint16_t FIXED_ATTR;
typedef svuint32_t fixed_uint32_t FIXED_ATTR;
typedef svuint64_t fixed_uint64_t FIXED_ATTR;

typedef svbfloat16_t fixed_bfloat16_t FIXED_ATTR;
typedef svfloat16_t fixed_float16_t FIXED_ATTR;
typedef svfloat32_t fixed_float32_t FIXED_ATTR;
typedef svfloat64_t fixed_float64_t FIXED_ATTR;

typedef svbool_t fixed_bool_t FIXED_ATTR;

typedef int8_t gnu_int8_t GNU_ATTR;
typedef int16_t gnu_int16_t GNU_ATTR;
typedef int32_t gnu_int32_t GNU_ATTR;
typedef int64_t gnu_int64_t GNU_ATTR;

typedef uint8_t gnu_uint8_t GNU_ATTR;
typedef uint16_t gnu_uint16_t GNU_ATTR;
typedef uint32_t gnu_uint32_t GNU_ATTR;
typedef uint64_t gnu_uint64_t GNU_ATTR;

typedef bfloat16_t gnu_bfloat16_t GNU_ATTR;
typedef float16_t gnu_float16_t GNU_ATTR;
typedef float32_t gnu_float32_t GNU_ATTR;
typedef float64_t gnu_float64_t GNU_ATTR;

void f() {
#define TEST_VECTOR(TYPE) \
  do \
    { \
      int assert_sizeof[sizeof (TYPE) == N / 8 ? 1 : -1]; \
      int assert_alignof[alignof (TYPE) == 16 ? 1 : -1]; \
    } \
  while (0)

  TEST_VECTOR (fixed_int8_t);
  TEST_VECTOR (fixed_int16_t);
  TEST_VECTOR (fixed_int32_t);
  TEST_VECTOR (fixed_int64_t);

  TEST_VECTOR (fixed_uint8_t);
  TEST_VECTOR (fixed_uint16_t);
  TEST_VECTOR (fixed_uint32_t);
  TEST_VECTOR (fixed_uint64_t);

  TEST_VECTOR (fixed_bfloat16_t);
  TEST_VECTOR (fixed_float16_t);
  TEST_VECTOR (fixed_float32_t);
  TEST_VECTOR (fixed_float64_t);

#undef TEST_VECTOR

  {
    int assert_sizeof[sizeof(fixed_bool_t) == N / 64 ? 1 : -1];
    int assert_alignof[alignof(fixed_bool_t) == 2 ? 1 : -1];
  }
}

#define TEST_GLOBAL(TYPE)			\
  extern fixed_##TYPE extern_##TYPE;		\
  fixed_##TYPE global_##TYPE;

#define TEST_STRUCT(TYPE)			\
  struct struct_##TYPE				\
  {						\
    fixed_##TYPE a, b, c[3];			\
  };						\
						\
  union union_##TYPE				\
  {						\
    fixed_##TYPE a, b, c[3];			\
  };

#define TEST_CONVERT(TYPE, PREFIX)		\
  PREFIX##TYPE					\
  to_##PREFIX##TYPE (fixed_##TYPE x)		\
  {						\
    return x;					\
  }						\
						\
  fixed_##TYPE					\
  from_##PREFIX##TYPE (PREFIX##TYPE x)		\
  {						\
    return x;					\
  }

#define TEST_UNARY(TYPE, NAME, OP)		\
  fixed_##TYPE					\
  NAME##_##TYPE (fixed_##TYPE x)		\
  {						\
    return OP x;				\
  }

#define TEST_BINARY(TYPE, NAME, OP)			\
  fixed_##TYPE						\
  NAME##_##TYPE (fixed_##TYPE x, fixed_##TYPE y)	\
  {							\
    return x OP y;					\
  }							\
							\
  fixed_##TYPE						\
  NAME##_##TYPE##_eq (fixed_##TYPE x, fixed_##TYPE y)	\
  {							\
    x OP##= y; return x;				\
  }

#define TEST_COMPARISON(TYPE, NAME, OP)			\
  fixed_##TYPE						\
  NAME##_##TYPE (fixed_##TYPE x, fixed_##TYPE y)	\
  {							\
    return x OP y;					\
  }

#define TEST_CALL(TYPE)							\
  fixed_##TYPE								\
  call_##TYPE##_ff (svbool_t pg, fixed_##TYPE x, fixed_##TYPE y)	\
  {									\
    return svsel (pg, x, y);						\
  }									\
									\
  fixed_##TYPE								\
  call_##TYPE##_sf (svbool_t pg, sv##TYPE x, fixed_##TYPE y)		\
  {									\
    return svsel (pg, x, y);						\
  }									\
									\
  fixed_##TYPE								\
  call_##TYPE##_fs (svbool_t pg, fixed_##TYPE x, sv##TYPE y)		\
  {									\
    return svsel (pg, x, y);						\
  }

#define TEST_COMMON(TYPE)			\
  TEST_GLOBAL (TYPE)				\
  TEST_STRUCT (TYPE)				\
  TEST_CONVERT (TYPE, sv)			\
  TEST_CALL (TYPE)

#define TEST_VECTOR(TYPE)			\
  TEST_COMMON (TYPE)				\
  TEST_CONVERT (TYPE, gnu_)			\
  TEST_UNARY (TYPE, nop, +)			\
  TEST_UNARY (TYPE, neg, -)			\
  TEST_BINARY (TYPE, add, +)			\
  TEST_BINARY (TYPE, sub, -)			\
  TEST_BINARY (TYPE, mul, *)			\
  TEST_BINARY (TYPE, div, /)			\

#define TEST_INT_VECTOR(TYPE)			\
  TEST_VECTOR (TYPE)				\
  TEST_UNARY (TYPE, inv, ~)			\
  TEST_BINARY (TYPE, mod, %)			\
  TEST_BINARY (TYPE, shl, <<)			\
  TEST_BINARY (TYPE, shr, >>)			\
  TEST_BINARY (TYPE, and, &)			\
  TEST_BINARY (TYPE, ior, |)			\
  TEST_BINARY (TYPE, xor, ^)			\
  TEST_COMPARISON (TYPE, eq, =)			\
  TEST_COMPARISON (TYPE, ne, !=)		\
  TEST_COMPARISON (TYPE, lt, <)			\
  TEST_COMPARISON (TYPE, le, <=)		\
  TEST_COMPARISON (TYPE, ge, >=)		\
  TEST_COMPARISON (TYPE, gt, >)

TEST_INT_VECTOR (int8_t);
TEST_INT_VECTOR (int16_t);
TEST_INT_VECTOR (int32_t);
TEST_INT_VECTOR (int64_t);

TEST_INT_VECTOR (uint8_t);
TEST_INT_VECTOR (uint16_t);
TEST_INT_VECTOR (uint32_t);
TEST_INT_VECTOR (uint64_t);

TEST_VECTOR (float16_t);
TEST_VECTOR (float32_t);
TEST_VECTOR (float64_t);

TEST_COMMON (bool_t)
