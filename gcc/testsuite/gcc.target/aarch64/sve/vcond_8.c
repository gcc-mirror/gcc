/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math -ffinite-math-only" } */

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

#define DEF_LOOP(TYPE, CMPTYPE, OP)				\
  void __attribute__((noipa))					\
  f_##OP##_##TYPE (TYPE *restrict dest, CMPTYPE *restrict cond,	\
		   CMPTYPE limit, TYPE *restrict src,		\
		   TYPE val, unsigned int n)			\
  {								\
    for (unsigned int i = 0; i < n; ++i)			\
      {								\
	TYPE truev = OP (src[i], val);				\
	dest[i] = cond[i] < limit ? truev : src[i];		\
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
  T (TYPE, CMPTYPE, div) \
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

/* { dg-final { scan-assembler-not {\tsel\t} } } */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.., z[0-9]+} } } */

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

/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.d, p[0-7]/m,} 1 } } */
