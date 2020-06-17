/* { dg-options "-Ofast -mlow-precision-div" } */

#define DEF_LOOP(TYPE)			\
  void					\
  test_##TYPE (TYPE *x, int n)		\
  {					\
    for (int i = 0; i < n; ++i)		\
      x[i] = (TYPE) 1 / x[i];		\
  }

#define TEST_ALL(T)	\
  T (_Float16)		\
  T (float)		\
  T (double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-not {\tfrecpe\tz[0-9]+\.h} } } */
/* { dg-final { scan-assembler-not {\tfrecps\tz[0-9]+\.h} } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tfrecpe\tz[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tfrecps\tz[0-9]+\.s} 1 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tfrecpe\tz[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\tfrecps\tz[0-9]+\.d} 2 } } */
