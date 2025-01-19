/* { dg-require-effective-target powerpc_vsx } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

/* Test we use xvcmpge[sd]p rather than xvcmpeq[sd]p and xvcmpgt[sd]p
   for UNGT and LE handlings.  */

#define UNGT(a, b) (!__builtin_islessequal ((a), (b)))
#define LE(a, b) (((a) <= (b)))

#define TEST_VECT(NAME, TYPE)                                                  \
  __attribute__ ((noipa)) void test_##NAME##_##TYPE (TYPE *x, TYPE *y,         \
						     int *res, int n)          \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      res[i] = NAME (x[i], y[i]);                                              \
  }

#define TEST(TYPE)                                                             \
  TEST_VECT (UNGT, TYPE)                                                       \
  TEST_VECT (LE, TYPE)

TEST (float)
TEST (double)

/* { dg-final { scan-assembler-not {\mxvcmp(gt|eq)[sd]p\M} } } */
