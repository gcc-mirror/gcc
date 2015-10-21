/* { dg-do compile } */
/* { dg-options "-O -g -fdump-tree-backprop-details" }  */

/* Test a loop that does both a multiplication and addition.  The addition
   should prevent any sign ops from being removed.  */
#define TEST_FUNCTION(TYPE, SUFFIX)			\
  TYPE							\
  test##SUFFIX (TYPE x, TYPE y, TYPE *array, int n)	\
  {							\
    x = __builtin_copysign##SUFFIX (x, y);		\
    for (int i = 0; i < n; ++i)				\
      x = (x + 1) * array[i];				\
    return __builtin_hypot##SUFFIX (x, y);		\
  }

TEST_FUNCTION (float, f)
TEST_FUNCTION (double, )
TEST_FUNCTION (long double, l)

/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = __builtin_copysign} 0 "backprop" } } */
