#include "arm_neon.h"

#pragma GCC target "+simd+fp16+bf16+sha3+aes"

#define VEC_LEN(VEC) (sizeof (VEC) / sizeof (VEC[0]))

#define TEST_UNARY(NAME, RET_TYPE, ARG_1_TYPE)                                 \
  RET_TYPE test_##NAME (ARG_1_TYPE a) { return NAME (a); }

#define TEST_UNIFORM_UNARY(NAME, TYPE) TEST_UNARY (NAME, TYPE, TYPE)

#define TEST_BINARY(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE)                    \
  RET_TYPE test_##NAME (ARG_1_TYPE a, ARG_2_TYPE b) { return NAME (a, b); }

#define TEST_UNIFORM_BINARY(NAME, TYPE) TEST_BINARY (NAME, TYPE, TYPE, TYPE)

#define TEST_TERNARY(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE, ARG_3_TYPE)       \
  RET_TYPE test_##NAME (ARG_1_TYPE a, ARG_2_TYPE b, ARG_3_TYPE c)              \
  {                                                                            \
    return NAME (a, b, c);                                                     \
  }

#define TEST_UNIFORM_TERNARY(NAME, TYPE)                                       \
  TEST_TERNARY (NAME, TYPE, TYPE, TYPE, TYPE)
