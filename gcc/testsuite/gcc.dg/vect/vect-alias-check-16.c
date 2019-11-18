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

#define TEST_VALUE(I) ((I) * 13 / 2)

#define ADD_TEST(TYPE)				\
  TYPE __attribute__((noinline, noclone))	\
  test_##TYPE (TYPE *x, TYPE *y)		\
  {						\
    TYPE res = 0;				\
    for (int i = 0; i < N; ++i)			\
      {						\
	x[i] = i;				\
	res += y[i];				\
      }						\
    return res;					\
  }

#define DO_TEST(TYPE)						\
  for (int i = 0; i < DIST * 2; ++i)				\
    {								\
      TYPE a[N + DIST * 2];					\
      for (int j = 0; j < N + DIST * 2; ++j)			\
	a[j] = TEST_VALUE (j);					\
      TYPE res = test_##TYPE (a + DIST, a + i);			\
      for (int j = 0; j < N; ++j)				\
	if (a[j + DIST] != (TYPE) j)				\
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
/* { dg-final { scan-tree-dump "using an address-based overlap test" "vect" { target { ! vect_check_ptrs } } } } */
/* { dg-final { scan-tree-dump "using an IFN_CHECK_RAW_PTRS test" "vect" { target vect_check_ptrs } } } */
/* { dg-final { scan-tree-dump-not "using an index-based" "vect" } } */
