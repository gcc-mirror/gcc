/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Expliot vector absolute difference unsigned.  */

#define N 128
#define PRAGMA(X) _Pragma (#X)
#define UNROLL0 PRAGMA (GCC unroll 0)

#define TEST1(TYPE)                                                            \
  void test1_##TYPE (unsigned TYPE *restrict a, unsigned TYPE *restrict b,     \
		     unsigned TYPE *restrict out)                              \
  {                                                                            \
    UNROLL0                                                                    \
    for (int i = 0; i < N; i++)                                                \
      out[i] = __builtin_abs (a[i] - b[i]);                                    \
  }

TEST1(char)
TEST1(short)

#define TEST2(TYPE1, TYPE2, FUNC)                                              \
  void test2_##TYPE1 (unsigned TYPE1 *restrict a, unsigned TYPE1 *restrict b,  \
		      unsigned TYPE1 *restrict out)                            \
  {                                                                            \
    UNROLL0                                                                    \
    for (int i = 0; i < N; i++)                                                \
      out[i] = __builtin_##FUNC ((TYPE2) a[i] - (TYPE2) b[i]);                 \
  }

TEST2(char, int, abs)
TEST2(short, int, abs)
TEST2(int, long long, llabs)

/* { dg-final { scan-assembler-times {\mvabsdub\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvabsduh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvabsduw\M} 1 } } */
