#include "arm_neon.h"

#pragma GCC target "+simd+fp16+bf16+sha2+sha3+aes+sm4"

#define VEC_LEN(VEC) (sizeof (VEC) / sizeof (VEC[0]))

#define TEST_UNARY(NAME, RET_TYPE, ARG_1_TYPE)                                 \
  RET_TYPE test_##NAME (ARG_1_TYPE a) { return NAME (a); }

#define TEST_UNIFORM_UNARY(NAME, TYPE) TEST_UNARY (NAME, TYPE, TYPE)

#define TEST_UNARY_N(NAME, RET_TYPE, ARG_1_TYPE, N)                            \
  RET_TYPE test_##NAME##_##N (ARG_1_TYPE a) { return NAME (a, N); }

#define TEST_UNIFORM_UNARY_N(NAME, TYPE, N) TEST_UNARY_N (NAME, TYPE, TYPE, N)

#define TEST_BINARY(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE)                    \
  RET_TYPE test_##NAME (ARG_1_TYPE a, ARG_2_TYPE b) { return NAME (a, b); }

#define TEST_BINARY_LANE(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE)               \
  RET_TYPE test_##NAME (ARG_1_TYPE a, ARG_2_TYPE b)                            \
  {                                                                            \
    return NAME (a, b, VEC_LEN (b) - 1);                                       \
  }

#define TEST_UNIFORM_BINARY(NAME, TYPE) TEST_BINARY (NAME, TYPE, TYPE, TYPE)

#define TEST_BINARY_N(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE, N)               \
  RET_TYPE test_##NAME##_##N (ARG_1_TYPE a, ARG_2_TYPE b)                      \
  {                                                                            \
    return NAME (a, b, N);                                                     \
  }

#define TEST_UNIFORM_BINARY_N(NAME, TYPE, N)                                   \
  TEST_BINARY_N (NAME, TYPE, TYPE, TYPE, N)

#define TEST_TERNARY(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE, ARG_3_TYPE)       \
  RET_TYPE test_##NAME (ARG_1_TYPE a, ARG_2_TYPE b, ARG_3_TYPE c)              \
  {                                                                            \
    return NAME (a, b, c);                                                     \
  }

#define TEST_UNIFORM_TERNARY(NAME, TYPE)                                       \
  TEST_TERNARY (NAME, TYPE, TYPE, TYPE, TYPE)
