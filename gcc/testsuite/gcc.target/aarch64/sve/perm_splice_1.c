/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -mautovec-preference=sve-only" } */
/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-additional-options "-fvect-cost-model=unlimited -fdump-tree-vect-details" } */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

#define DEF_RECUR1(TYPE, SUFFIX)                                     \
  __attribute__((noipa))                                             \
  void                                                               \
  recur1_##SUFFIX (TYPE *__restrict a, TYPE *__restrict b,           \
		   const TYPE *__restrict init)                         \
  {                                                                  \
    TYPE prev = *init;                                               \
    for (int i = 0; i < 67; ++i)                                    \
      {                                                              \
	b[i] = a[i] - prev;                                           \
	prev = a[i];                                                  \
      }                                                              \
  }

#define DEF_RECUR2(TYPE, SUFFIX)                                     \
  __attribute__((noipa))                                             \
  void                                                               \
  recur2_##SUFFIX (TYPE *__restrict a, TYPE *__restrict b,           \
		   const TYPE *__restrict init)                         \
  {                                                                  \
    TYPE prev0 = *init;                                              \
    TYPE prev1 = *init;                                              \
    for (int i = 0; i < 66; i += 2)                                 \
      {                                                              \
	b[i] = a[i] - prev0;                                          \
	prev0 = a[i];                                                 \
	b[i + 1] = a[i + 1] - prev1;                                 \
	prev1 = a[i + 1];                                             \
      }                                                              \
  }

DEF_RECUR1 (uint8_t, u8)
DEF_RECUR1 (uint16_t, u16)
DEF_RECUR1 (uint32_t, u32)
DEF_RECUR1 (uint64_t, u64)

DEF_RECUR2 (uint8_t, u8)
DEF_RECUR2 (uint16_t, u16)
DEF_RECUR2 (uint32_t, u32)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.b, vl1} 4 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.b, vl2} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.h, vl2} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s, vl2} 1 } } */
/* { dg-final { scan-assembler-times {\trev\tp[0-9]+\.b, p[0-9]+\.b} 2 } } */
/* { dg-final { scan-assembler-times {\trev\tp[0-9]+\.h, p[0-9]+\.h} 2 } } */
/* { dg-final { scan-assembler-times {\trev\tp[0-9]+\.s, p[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\trev\tp[0-9]+\.d, p[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\tsplice\tz[0-9]+\.b, p[0-9]+, z[0-9]+\.b, z[0-9]+\.b} 2 } } */
/* { dg-final { scan-assembler-times {\tsplice\tz[0-9]+\.h, p[0-9]+, z[0-9]+\.h, z[0-9]+\.h} 2 } } */
/* { dg-final { scan-assembler-times {\tsplice\tz[0-9]+\.s, p[0-9]+, z[0-9]+\.s, z[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tsplice\tz[0-9]+\.d, p[0-9]+, z[0-9]+\.d, z[0-9]+\.d} 1 } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 7 "vect" } } */
