/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-backprop-details" }  */

void start (void *);
void end (void *);

/* Test that we optimize the contents of infinite loops.  */
#define TEST_FUNCTION(TYPE, SUFFIX)			\
  void							\
  test##SUFFIX (TYPE *array, TYPE y, int n)		\
  {							\
    for (;;)						\
      {							\
	start (array);					\
	TYPE x = -__builtin_fabs##SUFFIX (array[-1]);	\
	for (int i = 0; i < n; ++i)			\
	  x = -x / array[i];				\
	array[-1] = x * x;				\
	array[-2] = __builtin_fma##SUFFIX (x, x, y);	\
	array[-3] = __builtin_pow##SUFFIX (x, 20);	\
	end (array);					\
      }							\
  }

TEST_FUNCTION (float, f)
TEST_FUNCTION (double, )
TEST_FUNCTION (long double, l)

/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = -} 6 "backprop" } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = ABS_EXPR <} 3 "backprop" } } */
