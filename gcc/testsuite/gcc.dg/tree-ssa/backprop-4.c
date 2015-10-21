/* { dg-do compile } */
/* { dg-options "-O -g -fdump-tree-backprop-details" }  */

/* Test a simple reduction loop in which all inputs are sign ops and
   the consumer doesn't care about the sign.  */
#define TEST_FUNCTION(TYPE, SUFFIX)			\
  TYPE							\
  test##SUFFIX (TYPE x, TYPE y, TYPE *array, int n)	\
  {							\
    x = __builtin_copysign##SUFFIX (x, y);		\
    for (int i = 0; i < n; ++i)				\
      x *= -array[i];					\
    return __builtin_hypot##SUFFIX (x, y);		\
  }

TEST_FUNCTION (float, f)
TEST_FUNCTION (double, )
TEST_FUNCTION (long double, l)

/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = __builtin_copysign} 3 "backprop" } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = -} 3 "backprop" } } */
