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

#define ADD_TEST(TYPE)				\
  TYPE a_##TYPE[N * 2];				\
  void __attribute__((noinline, noclone))	\
  test_##TYPE (int x, int y)			\
  {						\
    for (int i = 0; i < N; ++i)			\
      {						\
	a_##TYPE[i + x] = i;			\
	a_##TYPE[i + y] = 42 - i * 2;		\
      }						\
  }

#define DO_TEST(TYPE)						\
  for (int i = 0; i < DIST * 2; ++i)				\
    {								\
      __builtin_memset (a_##TYPE, 0, sizeof (a_##TYPE));	\
      test_##TYPE (DIST, i);					\
      for (int j = 0; j < N + DIST * 2; ++j)			\
	{							\
	  TYPE expected = 0;					\
	  if (i > DIST && j >= i && j < i + N)			\
	    expected = 42 - (j - i) * 2;			\
	  if (j >= DIST && j < DIST + N)			\
	    expected = j - DIST;				\
	  if (i <= DIST && j >= i && j < i + N)			\
	    expected = 42 - (j - i) * 2;			\
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

/* { dg-final { scan-tree-dump {flags: *WAW\n} "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump "using an index-based WAR/WAW test" "vect" } } */
/* { dg-final { scan-tree-dump-not "using an address-based" "vect" } } */
