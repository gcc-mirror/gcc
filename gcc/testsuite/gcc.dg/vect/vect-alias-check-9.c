#define N 200
#define M 4

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

#define TEST_VALUE(I) ((I) * 17 / 2)

#define ADD_TEST(TYPE)				\
  void __attribute__((noinline, noclone))	\
  test_##TYPE (TYPE *a, TYPE *b)		\
  {						\
    for (int i = 0; i < N; i += 2)		\
      {						\
	a[i + 0] = b[i + 0] + 2;		\
	a[i + 1] = b[i + 1] + 3;		\
      }						\
  }

#define DO_TEST(TYPE)					\
  for (int j = 1; j < M; ++j)				\
    {							\
      TYPE a[N + M];					\
      for (int i = 0; i < N + M; ++i)			\
	a[i] = TEST_VALUE (i);				\
      test_##TYPE (a + j, a);				\
      for (int i = 0; i < N; i += 2)			\
	if (a[i + j] != (TYPE) (a[i] + 2)		\
	    || a[i + j + 1] != (TYPE) (a[i + 1] + 3))	\
	  __builtin_abort ();				\
    }

FOR_EACH_TYPE (ADD_TEST)

int
main (void)
{
  FOR_EACH_TYPE (DO_TEST)
  return 0;
}

/* { dg-final { scan-tree-dump {flags: [^\n]*ARBITRARY\n} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump "using an address-based overlap test" "vect" } } */
/* { dg-final { scan-tree-dump-not "using an index-based" "vect" } } */
