/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define N 100

#define eq(A, B) ((A) == (B))
#define ne(A, B) ((A) != (B))
#define lt(A, B) ((A) < (B))
#define le(A, B) ((A) <= (B))
#define ge(A, B) ((A) >= (B))
#define gt(A, B) ((A) > (B))
#define unordered(A, B) (__builtin_isunordered (A, B))

#define DEF_CONST_LOOP(NAME, SUFFIX, TYPE, CONST)			\
  void __attribute__ ((noipa))						\
  NAME##_##SUFFIX##_##TYPE (TYPE *restrict dst, TYPE *restrict src)	\
  {									\
    for (int i = 0; i < N; ++i)						\
      if (NAME (src[i], CONST))						\
	dst[i] = 1;							\
  }

#define DEF_LOOP(NAME, TYPE, CONST1, CONST2)				\
  void __attribute__ ((noipa))						\
  NAME##_var_##TYPE (TYPE *restrict dst, TYPE *restrict src, TYPE x)	\
  {									\
    for (int i = 0; i < N; ++i)						\
      if (NAME (src[i], x))						\
	dst[i] = x;							\
  }									\
  DEF_CONST_LOOP (NAME, const1, TYPE, CONST1)				\
  DEF_CONST_LOOP (NAME, const2, TYPE, CONST2)

#define FOR_EACH_INT_OPERATOR(T, TYPE, CONST1, CONST2) \
  T (eq, TYPE, CONST1, CONST2) \
  T (ne, TYPE, CONST1, CONST2) \
  T (le, TYPE, CONST1, CONST2) \
  T (lt, TYPE, CONST1, CONST2) \
  T (gt, TYPE, CONST1, CONST2) \
  T (ge, TYPE, CONST1, CONST2)

#define FOR_EACH_FLOAT_OPERATOR(T, TYPE, CONST1, CONST2) \
  FOR_EACH_INT_OPERATOR(T, TYPE, CONST1, CONST2) \
  T (unordered, TYPE, CONST1, CONST2)

#define FOR_EACH_TYPE(T) \
  FOR_EACH_INT_OPERATOR (T, int8_t, 2, 100) \
  FOR_EACH_INT_OPERATOR (T, int16_t, 3, 1000) \
  FOR_EACH_INT_OPERATOR (T, int32_t, 4, 2000) \
  FOR_EACH_INT_OPERATOR (T, int64_t, 5, 3000) \
  FOR_EACH_INT_OPERATOR (T, uint8_t, 2, 160) \
  FOR_EACH_INT_OPERATOR (T, uint16_t, 3, 500) \
  FOR_EACH_INT_OPERATOR (T, uint32_t, 4, 1500) \
  FOR_EACH_INT_OPERATOR (T, uint64_t, 5, 2500) \
  FOR_EACH_FLOAT_OPERATOR (T, _Float16, 0, 1) \
  FOR_EACH_FLOAT_OPERATOR (T, float, 0, 1) \
  FOR_EACH_FLOAT_OPERATOR (T, double, 0, 1)

FOR_EACH_TYPE (DEF_LOOP)

/* { dg-final { scan-assembler-not {\tand\t} } } */

/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #2\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #3\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #4\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #5\n} 2 } } */

/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #2\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #3\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #4\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #5\n} 2 } } */

/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #4\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #5\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #5\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #4\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #4\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #5\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #5\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #4\n} 1 } } */


/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfcmne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfcmle\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmle\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfcmge\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmge\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
