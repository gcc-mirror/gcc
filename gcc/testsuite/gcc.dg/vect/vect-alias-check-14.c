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
	TYPE b0 = b[i + 0];			\
	TYPE b1 = b[i + 1];			\
	a[i + 0] = b0 + 2;			\
	a[i + 1] = b1 + 3;			\
      }						\
  }

#define DO_TEST(TYPE)						\
  for (int j = 0; j < M; ++j)					\
    {								\
      TYPE a[N + M];						\
      for (int i = 0; i < N + M; ++i)				\
	a[i] = TEST_VALUE (i);					\
      test_##TYPE (a + j, a);					\
      for (int i = 0; i < N; i += 2)				\
	{							\
	  TYPE base1 = j == 0 ? TEST_VALUE (i) : a[i];		\
	  TYPE base2 = j <= 1 ? TEST_VALUE (i + 1) : a[i + 1];	\
	  if (a[i + j] != (TYPE) (base1 + 2)			\
	      || a[i + j + 1] != (TYPE) (base2 + 3))		\
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
/* { dg-final { scan-tree-dump-not {flags: [^\n]*ARBITRARY\n} "vect" } } */
/* { dg-final { scan-tree-dump "using an address-based WAR/WAW test" "vect" { target { ! vect_check_ptrs } } } } */
/* { dg-final { scan-tree-dump "using an IFN_CHECK_WAR_PTRS test" "vect" { target vect_check_ptrs } } } */
/* { dg-final { scan-tree-dump-not "using an index-based" "vect" } } */
