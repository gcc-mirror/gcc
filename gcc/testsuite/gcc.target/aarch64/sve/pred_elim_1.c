/* { dg-options "-O2 -ftree-vectorize" } */

#define TEST_OP(NAME, TYPE, OP)				\
  void							\
  NAME##_##TYPE (TYPE *restrict a, TYPE *restrict b,	\
		 TYPE *restrict c, int n)		\
  {							\
    for (int i = 0; i < n; ++i)				\
      a[i] = b[i] OP c[i];				\
  }

#define TEST_TYPE(TYPE) \
  TEST_OP (add, TYPE, +) \
  TEST_OP (sub, TYPE, -) \
  TEST_OP (mult, TYPE, *) \

TEST_TYPE (float)
TEST_TYPE (double)

/* { dg-final { scan-assembler-times {\tfadd\t} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\t} 2 } } */
/* { dg-final { scan-assembler-times {\tfmul\t} 2 } } */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */
