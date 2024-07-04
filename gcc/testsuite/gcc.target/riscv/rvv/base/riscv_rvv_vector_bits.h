#ifndef HAVE_DEF_RISCV_RVV_VECTOR_BITS_H
#define HAVE_DEF_RISCV_RVV_VECTOR_BITS_H

#include "riscv_vector.h"
#include "../autovec/vls/def.h"

#define DEF_FIXED_TYPE(T, N)                                                   \
  typedef T fixed_##T __attribute__((riscv_rvv_vector_bits(N)));

#define DEF_FIXED_GLOBAL_VAR(T, N)                                             \
  fixed_##T global_fixed_##T##_##N##_bits_var;

#define DEF_FIXED_STRUCT_TYPE(T, N)                                            \
  struct fixed_##T##_##N##_bits_struct                                         \
  {                                                                            \
    fixed_##T a, b[2];                                                         \
  };

#define DEF_FIXED_UNION_TYPE(T, N)                                             \
  union fixed_##T##_##N##_bits_union                                           \
  {                                                                            \
    fixed_##T a, b[3];                                                         \
  };

#define DEF_FIXED_TYPE_SIZE(T, N)                                              \
  void test_fixed_##T##_##N##_bits_size ()                                     \
  {                                                                            \
    _Static_assert (sizeof (fixed_##T) * 8 == (N < 8 ? 8 : N),                 \
		    "Fixed RVV register types should be equal.");              \
  }

#define DEF_FIXED_TYPE_CVT(T, N, NEW_TYPE)                                     \
  NEW_TYPE test_fixed_##T##_##N##_bits_cvt (fixed_##T a)                       \
  {                                                                            \
    return (NEW_TYPE) a;                                                       \
  }

#define DEF_FIXED_BINARY(T, N, OP, NAME)                                       \
  fixed_##T test_fixed_##T##_##N##_bits_binary_##NAME (fixed_##T a,            \
						       fixed_##T b)            \
  {                                                                            \
    return a OP b;                                                             \
  }

#define DEF_FIXED_UNARY(T, N, OP, NAME)                                        \
  fixed_##T test_fixed_##T##_##N##_bits_unary_##NAME (fixed_##T a)             \
  {                                                                            \
    return OP a;                                                               \
  }

#define DEF_FIXED_TYPE_CMP(T, N)                                               \
  DEF_FIXED_BINARY(T, N, ==, eq)                                               \
  DEF_FIXED_BINARY(T, N, !=, ne)                                               \
  DEF_FIXED_BINARY(T, N, >,  gt)                                               \
  DEF_FIXED_BINARY(T, N, <,  lt)                                               \
  DEF_FIXED_BINARY(T, N, >=, ge)                                               \
  DEF_FIXED_BINARY(T, N, <=, le)

#define DEF_FIXED_TYPE_INT_ALU(T, N)                                           \
  DEF_FIXED_BINARY(T, N, +,  add)                                              \
  DEF_FIXED_BINARY(T, N, -,  sub)                                              \
  DEF_FIXED_BINARY(T, N, *,  mul)                                              \
  DEF_FIXED_BINARY(T, N, /,  div)                                              \
  DEF_FIXED_BINARY(T, N, %,  mod)                                              \
  DEF_FIXED_BINARY(T, N, &,  and)                                              \
  DEF_FIXED_BINARY(T, N, |,  ior)                                              \
  DEF_FIXED_BINARY(T, N, ^,  xor)                                              \
  DEF_FIXED_BINARY(T, N, >>, rsh)                                              \
  DEF_FIXED_BINARY(T, N, <<, lsh)                                              \
  DEF_FIXED_UNARY(T, N, ~, not)                                                \
  DEF_FIXED_UNARY(T, N, -, neg)

#define DEF_FIXED_TYPE_FLOAT_ALU(T, N)                                         \
  DEF_FIXED_BINARY(T, N, +,  add)                                              \
  DEF_FIXED_BINARY(T, N, -,  sub)                                              \
  DEF_FIXED_BINARY(T, N, *,  mul)                                              \
  DEF_FIXED_BINARY(T, N, /,  div)                                              \
  DEF_FIXED_UNARY(T, N, -,  neg)

#define TEST_FIXED_TYPE_INT_ALL(T, N, NEW_TYPE)                                \
  DEF_FIXED_TYPE (T, N)                                                        \
  DEF_FIXED_TYPE_SIZE (T, N)                                                   \
  DEF_FIXED_GLOBAL_VAR (T, N)                                                  \
  DEF_FIXED_STRUCT_TYPE (T, N)                                                 \
  DEF_FIXED_UNION_TYPE (T, N)                                                  \
  DEF_FIXED_TYPE_CVT (T, N, NEW_TYPE)                                          \
  DEF_FIXED_TYPE_CMP (T, N)                                                    \
  DEF_FIXED_TYPE_INT_ALU (T, N)                                                \

#define TEST_FIXED_TYPE_FLOAT_ALL(T, N, NEW_TYPE)                              \
  DEF_FIXED_TYPE (T, N)                                                        \
  DEF_FIXED_TYPE_SIZE (T, N)                                                   \
  DEF_FIXED_GLOBAL_VAR (T, N)                                                  \
  DEF_FIXED_STRUCT_TYPE (T, N)                                                 \
  DEF_FIXED_UNION_TYPE (T, N)                                                  \
  DEF_FIXED_TYPE_CVT (T, N, NEW_TYPE)                                          \
  DEF_FIXED_TYPE_CMP (T, N)                                                    \
  DEF_FIXED_TYPE_FLOAT_ALU (T, N)                                              \

#define TEST_FIXED_TYPE_BOOL_ALL(T, N, NEW_TYPE)                               \
  DEF_FIXED_TYPE (T, N)                                                        \
  DEF_FIXED_TYPE_SIZE (T, N)                                                   \
  DEF_FIXED_GLOBAL_VAR (T, N)                                                  \
  DEF_FIXED_STRUCT_TYPE (T, N)                                                 \
  DEF_FIXED_UNION_TYPE (T, N)                                                  \
  DEF_FIXED_TYPE_CVT (T, N, NEW_TYPE)

#endif
