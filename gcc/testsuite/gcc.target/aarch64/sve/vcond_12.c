/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -fschedule-insns" } */

#include <stdint.h>

#define add(A, B) ((A) + (B))
#define sub(A, B) ((A) - (B))
#define mul(A, B) ((A) * (B))
#define div(A, B) ((A) / (B))
#define max(A, B) ((A) > (B) ? (A) : (B))
#define min(A, B) ((A) < (B) ? (A) : (B))
#define and(A, B) ((A) & (B))
#define ior(A, B) ((A) | (B))
#define xor(A, B) ((A) ^ (B))

#define N 121

#define DEF_LOOP(TYPE, CMPTYPE, OP)				\
  void __attribute__((noipa))					\
  f_##OP##_##TYPE (TYPE *restrict dest, CMPTYPE *restrict cond,	\
		   CMPTYPE limit, TYPE src2v, TYPE elsev)	\
  {								\
    TYPE induc = 0;						\
    for (unsigned int i = 0; i < N; ++i, induc += 1)		\
      {								\
	TYPE truev = OP (induc, src2v);				\
	dest[i] = cond[i] < limit ? truev : elsev;		\
      }								\
  }

#define FOR_EACH_INT_TYPE(T, TYPE) \
  T (TYPE, TYPE, add) \
  T (TYPE, TYPE, sub) \
  T (TYPE, TYPE, mul) \
  T (TYPE, TYPE, max) \
  T (TYPE, TYPE, min) \
  T (TYPE, TYPE, and) \
  T (TYPE, TYPE, ior) \
  T (TYPE, TYPE, xor)

#define FOR_EACH_FP_TYPE(T, TYPE, CMPTYPE, SUFFIX) \
  T (TYPE, CMPTYPE, add) \
  T (TYPE, CMPTYPE, sub) \
  T (TYPE, CMPTYPE, mul) \
  /* No div because that gets converted into a mul anyway.  */ \
  T (TYPE, CMPTYPE, __builtin_fmax##SUFFIX) \
  T (TYPE, CMPTYPE, __builtin_fmin##SUFFIX)

#define FOR_EACH_LOOP(T) \
  FOR_EACH_INT_TYPE (T, int8_t) \
  FOR_EACH_INT_TYPE (T, int16_t) \
  FOR_EACH_INT_TYPE (T, int32_t) \
  FOR_EACH_INT_TYPE (T, int64_t) \
  FOR_EACH_INT_TYPE (T, uint8_t) \
  FOR_EACH_INT_TYPE (T, uint16_t) \
  FOR_EACH_INT_TYPE (T, uint32_t) \
  FOR_EACH_INT_TYPE (T, uint64_t) \
  FOR_EACH_FP_TYPE (T, _Float16, uint16_t, f16) \
  FOR_EACH_FP_TYPE (T, float, float, f32) \
  FOR_EACH_FP_TYPE (T, double, double, f64)

FOR_EACH_LOOP (DEF_LOOP)

/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.., z[0-9]+} } } */

/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.b,} 16 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.h,} 21 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.s,} 21 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.d,} 21 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.d, p[0-7]/m,} 1 } } */
