/* Test fold-const.c (fold_range_test) optimizations.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#if (INT_MAX == 2147483647) && (INT_MIN == -2147483648) \
    && (SCHAR_MIN == -128) && (SCHAR_MAX == 127) \
    && (UCHAR_MIN == 0) && (UCHAR_MAX == 255)

#ifndef T

enum integers
{
  int_smallest = INT_MIN,
  int_2ndsmallest = INT_MIN + 1,
  int_3rdsmallest = INT_MIN + 2,
  int_minus2 = -2,
  int_minus1 = -1,
  int_zero = 0,
  int_one = 1,
  int_two = 2,
  int_3rdlargest = INT_MAX - 2,
  int_2ndlargest = INT_MAX - 1,
  int_largest = INT_MAX
};

int var;
void
check (void)
{
  ++var;
}

#define T(IDX, TYPE, TEST, YESARR, NOARR)				\
void __attribute__((noinline))						\
test##IDX (TYPE x)							\
{									\
  if (TEST)								\
    check ();								\
}
#include "range-test-1.c"
#undef T

int
main (void)
{
  int i, fails = 0;

#define C ,
#define T(IDX, TYPE, TEST, YESARR, NOARR)				\
  {									\
    static TYPE yesarr##IDX [] = YESARR;				\
    static TYPE noarr##IDX [] = NOARR;					\
    for (i = 0; i < (int) (sizeof (yesarr##IDX) / sizeof (TYPE)); ++i)	\
      {									\
	var = 0;							\
	test##IDX (yesarr##IDX [i]);					\
	if (var != 1)							\
	  printf ("test" #IDX " failed for yesarr [%u]\n", i), ++fails;	\
      }									\
    var = 0;								\
    for (i = 0; i < (int) (sizeof (noarr##IDX) / sizeof (TYPE)); ++i)	\
      {									\
	test##IDX (noarr##IDX [i]);					\
	if (var != 0)							\
	  printf ("test" #IDX " failed for noarr [%u]\n", i), ++fails;	\
      }									\
  }
#include "range-test-1.c"
#undef T

  if (fails)
    abort ();

  exit (0);
}

#else

/* Use `C' instead of `,' below to separate array entries.  */

/* These ought to be all optimized into single comparison.  */
T(1, unsigned int, x == 0 || x == 1,
  { 0 C 1 }, { -1U C 2 C 12 C 35 C 0x7fffffff C 0x80000000 })
T(2, unsigned int, x == 0 || x == -1U || x == -2U,
  { 0 C -1U C -2U }, { -3U C -6U C 1 C 2 C 12 C 35 C 0x7fffffff C 0x80000000 })
T(3, unsigned int, x == 0 || x == 1 || x == 2,
  { 0 C 1 C 2 }, { -3U C -6U C -1U C -2U C 12 C 35 C 0x7fffffff C 0x80000000 })
T(4, unsigned int, x == 3 || x == 4 || x == 5 || x == 6,
  { 3 C 4 C 5 C 6 }, { -3U C 0 C 1 C 2 C 7 C 8 C 12 C 0x7fffffff C 0x80000000 })
T(5, unsigned int, x == -3U || x == -4U || x == -5U || x == -6U,
  { -3U C -4U C -5U C -6U }, { -7U C -8U C -2U C -1U C 1 C 2 C 0x7fffffff C 0x80000000 })
T(6, unsigned int, x == -3U || x == -4U || x == -5U,
  { -3U C -4U C -5U }, { -6U C -7U C -8U C -2U C -1U C 1 C 2 C 0x7fffffff C 0x80000000 })
T(7, char *, x == (char *) -3UL || x == (char *) -4UL || x == (char *) -5UL,
  { (char *) -3UL C (char *) -4UL C (char *) -5UL },
  { (char *) -6UL C (char *) -20UL C (char *) -2UL C (char *) -1UL C (char *) 0
    C (char *) 1UL C (char *) 35UL C (char *) 0x7fffffffUL C (char *) 0x80000000UL })
T(8, unsigned long, x == -2UL || x == -1UL || x == 0,
  { 0 C -1UL C -2UL }, { -3UL C -6UL C 1 C 2 C 12 C 35 C 0x7fffffff C 0x80000000 })
T(9, unsigned long, x >= -4UL || x <= 8,
  { -4UL C -3UL C -2UL C -1UL C 0 C 1 C 2 C 3 C 4 C 5 C 6 C 7 C 8 },
  { -7UL C -5UL C 9 C 10 C 61 C 127 C 0x7fffffff C 0x80000000 })
T(10, signed char, x == 0 || x == -1 || x == -2 || x == -3,
  { 0 C -1 C -2 C -3 }, { -4 C -5 C 1 C 2 C 3 C 35 C -24 })
T(11, int, x == 0 || x == 1,
  { 0 C 1 }, { -1 C 2 C 12 C 35 C INT_MAX C INT_MIN })
T(12, int, x == 0 || x == -1 || x == -2,
  { 0 C -1 C -2 }, { -3 C -6 C 1 C 2 C 12 C 35 C INT_MAX C INT_MIN })
T(13, int, x == 0 || x == 1 || x == 2,
  { 0 C 1 C 2 }, { -3 C -6 C -1 C -2 C 12 C 35 C INT_MAX C INT_MIN })
T(14, int, x == 3 || x == 4 || x == 5 || x == 6,
  { 3 C 4 C 5 C 6 }, { -3 C 0 C 1 C 2 C 7 C 8 C 12 C INT_MAX C INT_MIN })
T(15, int, x == -3 || x == -4 || x == -5 || x == -6,
  { -3 C -4 C -5 C -6 }, { -7 C -8 C -2 C -1 C 1 C 2 C INT_MAX C INT_MIN })
T(16, int, x == -3 || x == -4 || x == -5,
  { -3 C -4 C -5 }, { -6 C -7 C -8 C -2 C -1 C 1 C 2 C INT_MAX C INT_MIN })
T(17, unsigned int, (x >= -8U && x <= -3U) || x == -2U || x == -1U || x == 0 || x == 1 || x == 2,
  { -8U C -7U C -6U C -5U C -4U C -3U C -2U C -1U C 0 C 1 C 2 },
  { -9U C -10U C 3 C 4 C 12 C -54U C INT_MAX C INT_MIN })
T(18, int, (x >= -8 && x <= -3) || x == -2 || x == -1 || x == 0 || x == 1 || x == 2,
  { -8 C -7 C -6 C -5 C -4 C -3 C -2 C -1 C 0 C 1 C 2 },
  { -9 C -10 C 3 C 4 C 12 C -54 C INT_MAX C INT_MIN })
T(19, unsigned long, (x >= 0 && x <= 16) || (x >= 18 && x <= -1UL),
  { -3UL C -6UL C -1UL C 0 C 1 C 2 C 12 C 15 C 16 C 18 C 19 C 35 C 0x7fffffff
    C 0x80000000 }, { 17 })
T(20, char *, x == (char *) -1UL || x == 0,
  { (char *) -1UL C 0 }, { (char *) -6UL C (char *) -20UL C (char *) -2UL
    C (char *) 1UL C (char *) 35UL C (char *) 0x7fffffffUL C (char *) 0x80000000UL })
T(21, enum integers, x == int_zero || x == int_one,
  { int_zero C int_one }, { int_minus1 C int_two C 12 C 35 C int_largest C int_smallest })
T(22, int, x == INT_MIN || x == INT_MAX,
  { INT_MIN C INT_MAX },
  { -1 C 0 C 1 C INT_MAX - 1 C INT_MAX - 2 C INT_MIN + 1 C INT_MIN + 2 })
T(23, int, x == INT_MIN + 1 || x == INT_MIN + 2 || x == INT_MIN || x == INT_MAX,
  { INT_MIN + 1 C INT_MIN + 2 C INT_MIN C INT_MAX },
  { -1 C 0 C 1 C INT_MAX - 1 C INT_MAX - 2 C INT_MIN + 3 C INT_MIN + 4 })
T(24, signed char, x == SCHAR_MIN || x == SCHAR_MAX,
  { SCHAR_MIN C SCHAR_MAX },
  { -1 C 0 C 1 C SCHAR_MAX - 1 C SCHAR_MAX - 2 C SCHAR_MIN + 1 C SCHAR_MIN + 2 })
T(25, enum integers, x == int_smallest || x == int_largest,
  { int_smallest C int_largest }, { int_minus1 C int_zero C int_one
    C int_2ndsmallest C int_2ndlargest C int_3rdsmallest C int_3rdlargest })

/* This should be optimized into unconditional jump.  */
T(o1, unsigned long, (x >= 0 && x <= 16) || (x >= 17 && x <= -1UL),
  { -3UL C -6UL C -1UL C 0 C 1 C 2 C 12 C 15 C 16 C 17 C 18 C 19 C 35 C 0x7fffffff
    C 0x80000000 }, { })
T(o2, unsigned long, (x >= 0 && x <= -3UL) || (x == -2UL || x == -1UL),
  { -3UL C -6UL C -1UL C 0 C 1 C 2 C 12 C 15 C 16 C 17 C 18 C 19 C 35 C 0x7fffffff
    C 0x80000000 }, { })

/* This should be eventually optimized into a single comparison.  */
T(td1, unsigned char, x == 0 || x == 4 || x == 1 || x == 5 || x == 2 || x == 6 || x == 3,
  { 0 C 1 C 2 C 3 C 4 C 5 C 6 }, { 7 C 8 C 127 C 128 C 254 C 255 })

#endif

#else
int
main (void)
{
  return 0;
}
#endif
