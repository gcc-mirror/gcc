// Test fold-const.c (fold_range_test) optimizations.
// { dg-do run } */
// { dg-options "-O2" } */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#if (INT_MAX == 2147483647) && (INT_MIN == -2147483648) \
    && (SCHAR_MIN == -128) && (SCHAR_MAX == 127) \
    && (UCHAR_MIN == 0) && (UCHAR_MAX == 255)

#ifndef T

enum enum3
{
  enum3_zero,
  enum3_one,
  enum3_two,
  enum3_three,
  enum3_four,
  enum3_five,
  enum3_six,
  enum3_seven
};

int var;
void
check ()
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
#include "range-test-2.C"
#undef T

int
main ()
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
#include "range-test-2.C"
#undef T

  if (fails)
    abort ();

  exit (0);
}

#else

/* Use `C' instead of `,' below to separate array entries.  */

T(26, enum3, x == enum3_one || x == enum3_two || x == enum3_three,
  { enum3_one C enum3_two C enum3_three }, { enum3_zero C enum3_four
    C enum3_five C enum3_six C enum3_seven })

#endif

#else

int
main (void)
{
  return 0;
}

#endif
