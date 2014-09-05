/*  test_vcXXX wrappers for all the vcXXX (vector compare) and vtst intrinsics
    in arm_neon.h (excluding the 64x1 variants as these generally produce scalar
    not vector ops).  */
#include "arm_neon.h"

#define DONT_FORCE(X)

#define FORCE_SIMD(V1)   asm volatile ("mov %d0, %1.d[0]"       \
           : "=w"(V1)                                           \
           : "w"(V1)                                            \
           : /* No clobbers */);

#define OP1(SIZE, OP, BASETYPE, SUFFIX, FORCE) uint##SIZE##_t	\
test_v##OP##SUFFIX (BASETYPE##SIZE##_t a)			\
{								\
  uint##SIZE##_t res;						\
  FORCE (a);							\
  res = v##OP##SUFFIX (a);					\
  FORCE (res);							\
  return res;							\
}

#define OP2(SIZE, OP, BASETYPE, SUFFIX, FORCE) uint##SIZE##_t	\
test_v##OP##SUFFIX (BASETYPE##SIZE##_t a, BASETYPE##SIZE##_t b) \
{								\
  uint##SIZE##_t res;						\
  FORCE (a);							\
  FORCE (b);							\
  res = v##OP##SUFFIX (a, b);					\
  FORCE (res);							\
  return res;							\
}

#define UNSIGNED_OPS(SIZE, BASETYPE, SUFFIX, FORCE) \
OP2 (SIZE, tst, BASETYPE, SUFFIX, FORCE) \
OP1 (SIZE, ceqz, BASETYPE, SUFFIX, FORCE) \
OP2 (SIZE, ceq, BASETYPE, SUFFIX, FORCE) \
OP2 (SIZE, cge, BASETYPE, SUFFIX, FORCE) \
OP2 (SIZE, cgt, BASETYPE, SUFFIX, FORCE) \
OP2 (SIZE, cle, BASETYPE, SUFFIX, FORCE) \
OP2 (SIZE, clt, BASETYPE, SUFFIX, FORCE)

#define ALL_OPS(SIZE, BASETYPE, SUFFIX, FORCE) \
OP1 (SIZE, cgez, BASETYPE, SUFFIX, FORCE) \
OP1 (SIZE, cgtz, BASETYPE, SUFFIX, FORCE) \
OP1 (SIZE, clez, BASETYPE, SUFFIX, FORCE) \
OP1 (SIZE, cltz, BASETYPE, SUFFIX, FORCE) \
UNSIGNED_OPS (SIZE, BASETYPE, SUFFIX, FORCE)

ALL_OPS (8x8, int, _s8, DONT_FORCE)
ALL_OPS (16x4, int, _s16, DONT_FORCE)
ALL_OPS (32x2, int, _s32, DONT_FORCE)
ALL_OPS (64x1, int, _s64, DONT_FORCE)
ALL_OPS (64, int, d_s64, FORCE_SIMD)
ALL_OPS (8x16, int, q_s8, DONT_FORCE)
ALL_OPS (16x8, int, q_s16, DONT_FORCE)
ALL_OPS (32x4, int, q_s32, DONT_FORCE)
ALL_OPS (64x2, int, q_s64, DONT_FORCE)
UNSIGNED_OPS (8x8, uint, _u8, DONT_FORCE)
UNSIGNED_OPS (16x4, uint, _u16, DONT_FORCE)
UNSIGNED_OPS (32x2, uint, _u32, DONT_FORCE)
UNSIGNED_OPS (64x1, uint, _u64, DONT_FORCE)
UNSIGNED_OPS (64, uint, d_u64, FORCE_SIMD)
UNSIGNED_OPS (8x16, uint, q_u8, DONT_FORCE)
UNSIGNED_OPS (16x8, uint, q_u16, DONT_FORCE)
UNSIGNED_OPS (32x4, uint, q_u32, DONT_FORCE)
UNSIGNED_OPS (64x2, uint, q_u64, DONT_FORCE)

