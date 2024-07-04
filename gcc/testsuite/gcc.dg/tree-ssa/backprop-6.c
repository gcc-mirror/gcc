/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-backprop-details" }  */
/* { dg-additional-options "-msse -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

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

/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = -} 4 "backprop" { target { ifn_copysign && { ! { s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = \.COPYSIGN} 2 "backprop" { target { ifn_copysign && { ! { s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = ABS_EXPR <} 1 "backprop" { target { ifn_copysign && { ! { s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = \.COPYSIGN} 3 "backprop" { target { ifn_copysign && s390*-*-* } } } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = -} 6 "backprop" { target { ! ifn_copysign } } } } */
/* { dg-final { scan-tree-dump-times {Deleting[^\n]* = ABS_EXPR <} 3 "backprop" { target { ! ifn_copysign } } } } */
