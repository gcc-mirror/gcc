/* Originally from gcc.dg/vect/vect-alias-check-10.c.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=512" } */

#define N 87
#define M 6

typedef signed char sc;
typedef unsigned char uc;
typedef signed short ss;
typedef unsigned short us;
typedef int si;
typedef unsigned int ui;
typedef signed long long sll;
typedef unsigned long long ull;

#define FOR_EACH_TYPE(M) \
  M (sc) M (uc) \
  M (ss) M (us) \
  M (si) M (ui) \
  M (sll) M (ull) \
  M (float) M (double)

#define TEST_VALUE(I) ((I) * 5 / 2)

#define ADD_TEST(TYPE)				\
  void __attribute__((noinline, noclone))	\
  test_##TYPE (TYPE *a, int step)		\
  {						\
    for (int i = 0; i < N; ++i)			\
      {						\
	a[i * step + 0] = a[i * step + 0] + 1;	\
	a[i * step + 1] = a[i * step + 1] + 2;	\
	a[i * step + 2] = a[i * step + 2] + 4;	\
	a[i * step + 3] = a[i * step + 3] + 8;	\
      }						\
  }						\
  void __attribute__((noinline, noclone))	\
  ref_##TYPE (TYPE *a, int step)		\
  {						\
    for (int i = 0; i < N; ++i)			\
      {						\
	a[i * step + 0] = a[i * step + 0] + 1;	\
	a[i * step + 1] = a[i * step + 1] + 2;	\
	a[i * step + 2] = a[i * step + 2] + 4;	\
	a[i * step + 3] = a[i * step + 3] + 8;	\
	asm volatile ("");			\
      }						\
  }

#define DO_TEST(TYPE)					\
  for (int j = -M; j <= M; ++j)				\
    {							\
      TYPE a[N * M], b[N * M];				\
      for (int i = 0; i < N * M; ++i)			\
	a[i] = b[i] = TEST_VALUE (i);			\
      int offset = (j < 0 ? N * M - 4 : 0);		\
      test_##TYPE (a + offset, j);			\
      ref_##TYPE (b + offset, j);			\
      if (__builtin_memcmp (a, b, sizeof (a)) != 0)	\
	__builtin_abort ();				\
    }

FOR_EACH_TYPE (ADD_TEST)

int
main (void)
{
  FOR_EACH_TYPE (DO_TEST)
  return 0;
}
