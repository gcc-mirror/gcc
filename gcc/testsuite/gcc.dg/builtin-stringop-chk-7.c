/* Verify that -Wstringop-overflow doesn't cause false positives for
   anti-ranges.  Note that not all of the statements below result in
   the memset argument being represented as an anti-range.

   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow" } */

#define SCHAR_MAX __SCHAR_MAX__
#define UCHAR_MAX (SCHAR_MAX * 2 + 1)

#define SHRT_MAX  __SHRT_MAX__
#define USHRT_MAX (SHRT_MAX * 2 + 1)

#define INT_MAX   __INT_MAX__
#define UINT_MAX  (INT_MAX * 2U + 1)

#define LONG_MAX __LONG_MAX__
#define ULONG_MAX (LONG_MAX * 2LU + 1)

#define PTRDIFF_MAX __PTRDIFF_MAX__
#define SIZE_MAX    __SIZE_MAX__

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

#define TEST_AR_1(T, prefix)			\
  void test_ar_1_ ## prefix (void *d, T n)	\
  {						\
    if (n == prefix ## _MAX - 1)		\
      n = prefix ## _MAX - 2;			\
    __builtin_memset (d, 0, n);			\
  } typedef void dummy

#define TEST_AR_2(T, prefix)					\
  void test_ar_2_ ## prefix (void *d, T n)			\
  {								\
    if (prefix ## _MAX - 2 <= n && n <= prefix ## _MAX - 1)	\
      n = prefix ## _MAX - 3;					\
    __builtin_memset (d, 0, n);					\
  } typedef void dummy

/* Verify antirange where MIN == MAX.  */
TEST_AR_1 (signed char, SCHAR);
TEST_AR_1 (unsigned char, UCHAR);

TEST_AR_1 (short, SHRT);
TEST_AR_1 (unsigned short, USHRT);

TEST_AR_1 (int, INT);
TEST_AR_1 (unsigned, UINT);

TEST_AR_1 (long, LONG);
TEST_AR_1 (unsigned long, ULONG);

TEST_AR_1 (ptrdiff_t, PTRDIFF);
TEST_AR_1 (size_t, SIZE);

/* Verify antirange where MIN < MAX.  */
TEST_AR_2 (signed char, SCHAR);
TEST_AR_2 (unsigned char, UCHAR);

TEST_AR_2 (short, SHRT);
TEST_AR_2 (unsigned short, USHRT);

TEST_AR_2 (int, INT);
TEST_AR_2 (unsigned, UINT);

TEST_AR_2 (long, LONG);
TEST_AR_2 (unsigned long, ULONG);

TEST_AR_2 (ptrdiff_t, PTRDIFF);
TEST_AR_2 (size_t, SIZE);
