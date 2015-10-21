/* { dg-do compile } */
/* { dg-options "-O -g -fdump-tree-backprop-details" }  */

/* Test a simple case of non-looping code in which both uses ignore
   the sign but only one definition is a sign op.  */
#define TEST_FUNCTION(TYPE, SUFFIX)				\
  TYPE								\
  test##SUFFIX (TYPE x, int sel1, int sel2)			\
  {								\
    TYPE input = sel1 ? -x : x + 1;				\
    if (sel2)							\
      return __builtin_cos##SUFFIX (input);			\
    else							\
      return __builtin_cosh##SUFFIX (input);			\
  }

TEST_FUNCTION (float, f)
TEST_FUNCTION (double, )
TEST_FUNCTION (long double, l)

/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = -x} 3 "backprop" } } */
