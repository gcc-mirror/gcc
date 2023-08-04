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

#define TEST_VALUE1(I) ((I) * 5 / 2)
#define TEST_VALUE2(I) ((I) * 11 / 5)

#define ADD_TEST(TYPE)					\
  void __attribute__((noinline, noclone))		\
  test_##TYPE (TYPE *restrict a, TYPE *restrict b,	\
	       int step)				\
  {							\
    for (int i = 0; i < N; ++i)				\
      {							\
	TYPE r1 = a[i * step + 0] += 1;			\
	a[i * step + 1] += 2;				\
	a[i * step + 2] += 4;				\
	a[i * step + 3] += 8;				\
	b[i] += r1;					\
      }							\
  }							\
							\
  void __attribute__((noinline, noclone))		\
  ref_##TYPE (TYPE *restrict a, TYPE *restrict b,	\
	      int step)					\
  {							\
    for (int i = 0; i < N; ++i)				\
      {							\
	TYPE r1 = a[i * step + 0] += 1;			\
	a[i * step + 1] += 2;				\
	a[i * step + 2] += 4;				\
	a[i * step + 3] += 8;				\
	b[i] += r1;					\
	asm volatile ("");				\
      }							\
  }

#define DO_TEST(TYPE)					\
  _Pragma("GCC novector")				\
  for (int j = -M; j <= M; ++j)				\
    {							\
      TYPE a1[N * M], a2[N * M], b1[N], b2[N];		\
      for (int i = 0; i < N * M; ++i)			\
	a1[i] = a2[i] = TEST_VALUE1 (i);		\
      for (int i = 0; i < N; ++i)			\
	b1[i] = b2[i] = TEST_VALUE2 (i);		\
      int offset = (j < 0 ? N * M - 4 : 0);		\
      test_##TYPE (a1 + offset, b1, j);			\
      ref_##TYPE (a2 + offset, b2, j);			\
      if (__builtin_memcmp (a1, a2, sizeof (a1)) != 0)	\
	__builtin_abort ();				\
      if (__builtin_memcmp (b1, b2, sizeof (b1)) != 0)	\
	__builtin_abort ();				\
    }

FOR_EACH_TYPE (ADD_TEST)

int
main (void)
{
  FOR_EACH_TYPE (DO_TEST)
  return 0;
}

/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* is outside \(-2, 2\)} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* is outside \(-3, 3\)} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* is outside \(-4, 4\)} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump {run-time check [^\n]* abs \([^*]*\) >= 4} "vect" { target vect_int } } } */

/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 2[)]* is outside \(-4, 4\)} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 2[)]* is outside \(-6, 6\)} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 2[)]* is outside \(-8, 8\)} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump {run-time check [^\n]* abs \([^*]* \* 2[)]* >= 8} "vect" { target vect_int } } } */

/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 4[)]* is outside \(-8, 8\)} "vect" { target { vect_int || vect_float } } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 4[)]* is outside \(-12, 12\)} "vect" { target { vect_int || vect_float } } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 4[)]* is outside \(-16, 16\)} "vect" { target { vect_int || vect_float } } } } */
/* { dg-final { scan-tree-dump {run-time check [^\n]* abs \([^*]* \* 4[)]* >= 16} "vect" { target { vect_int || vect_float } } } } */

/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 8[)]* is outside \(-16, 16\)} "vect" { target vect_double } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 8[)]* is outside \(-24, 24\)} "vect" { target vect_double } } } */
/* { dg-final { scan-tree-dump {no alias between [^\n]* when [^\n]* step[^ ]* \* 8[)]* is outside \(-32, 32\)} "vect" { target vect_double } } } */
/* { dg-final { scan-tree-dump {run-time check [^\n]* abs \([^*]* \* 8[)]* >= 32} "vect" { target vect_double } } } */

/* { dg-final { scan-tree-dump-not "using an address-based" "vect" } } */
/* { dg-final { scan-tree-dump-not "using an index-based" "vect" } } */
