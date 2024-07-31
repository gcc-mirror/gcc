/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Expliot vector absolute difference unsigned.  */

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define N 128
#define PRAGMA(X) _Pragma (#X)
#define UNROLL0 PRAGMA (GCC unroll 0)

#define TEST(T)                                                                \
  void uabd_##T (unsigned T *restrict a, unsigned T *restrict b,               \
		 unsigned T *restrict out)                                     \
  {                                                                            \
    UNROLL0                                                                    \
    for (int i = 0; i < N; i++)                                                \
      out[i] = MAX (a[i], b[i]) - MIN (a[i], b[i]);                            \
  }

TEST(char)
TEST(short)
TEST(int)

/* { dg-final { scan-assembler-times {\mvabsdub\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvabsduh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvabsduw\M} 1 } } */
