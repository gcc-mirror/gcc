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
  TYPE __attribute__((noinline, noclone))	\
  test_##TYPE (int x, int y)			\
  {						\
    TYPE res = 0;				\
    for (int i = 0; i < N; ++i)			\
      {						\
	a_##TYPE[i + x] = i;			\
	res += a_##TYPE[i + y];			\
      }						\
    return res;					\
  }

#define DO_TEST(TYPE)						\
  for (int i = 0; i < DIST * 2; ++i)				\
    {								\
      for (int j = 0; j < N + DIST * 2; ++j)			\
	a_##TYPE[j] = TEST_VALUE (j);				\
      TYPE res = test_##TYPE (DIST, i);				\
      for (int j = 0; j < N; ++j)				\
	if (a_##TYPE[j + DIST] != (TYPE) j)			\
	  __builtin_abort ();					\
      TYPE expected_res = 0;					\
      for (int j = i; j < i + N; ++j)				\
	if (i <= DIST && j >= DIST && j < DIST + N)		\
	  expected_res += j - DIST;				\
	else							\
	  expected_res += TEST_VALUE (j);			\
      if (expected_res != res)					\
	__builtin_abort ();					\
    }

FOR_EACH_TYPE (ADD_TEST)

int
main (void)
{
  FOR_EACH_TYPE (DO_TEST)
  return 0;
}

/* { dg-final { scan-tree-dump {flags: *RAW\n} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump "using an index-based overlap test" "vect" } } */
/* { dg-final { scan-tree-dump-not "using an address-based" "vect" } } */
