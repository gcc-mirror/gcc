#define N 200
#define DIST 32

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

#define TEST_VALUE(I) ((I) * 11 / 2)

#define ADD_TEST(TYPE)				\
  TYPE a_##TYPE[N * 2];				\
  void __attribute__((noinline, noclone))	\
  test_##TYPE (int x, int y)			\
  {						\
    for (int i = 0; i < N; ++i)			\
      a_##TYPE[x - i] += a_##TYPE[y - i];	\
  }

#define DO_TEST(TYPE)						\
  for (int i = 0; i < DIST * 2; ++i)				\
    {								\
      for (int j = 0; j < N + DIST * 2; ++j)			\
	a_##TYPE[j] = TEST_VALUE (j);				\
      test_##TYPE (i + N - 1, DIST + N - 1);			\
      _Pragma("GCC novector")					\
      for (int j = 0; j < N + DIST * 2; ++j)			\
	{							\
	  TYPE expected;					\
	  if (j < i || j >= i + N)				\
	    expected = TEST_VALUE (j);				\
	  else if (i >= DIST)					\
	    expected = ((TYPE) TEST_VALUE (j)			\
			+ (TYPE) TEST_VALUE (j + DIST - i));	\
	  else							\
	    expected = ((TYPE) TEST_VALUE (j)			\
			+ a_##TYPE[j + DIST - i]);		\
	  if (expected != a_##TYPE[j])				\
	    __builtin_abort ();					\
	}							\
    }

FOR_EACH_TYPE (ADD_TEST)

int
main (void)
{
  FOR_EACH_TYPE (DO_TEST)
  return 0;
}

/* { dg-final { scan-tree-dump {flags: *WAR\n} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump "using an index-based WAR/WAW test" "vect" { target { vect_int && vect_perm } } } } */
/* { dg-final { scan-tree-dump-not "using an address-based" "vect" } } */
