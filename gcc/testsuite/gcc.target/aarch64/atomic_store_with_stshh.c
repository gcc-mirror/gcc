/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a -save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

/*
** testFun1:
** ...
**	stshh	keep
**	strb	w[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun1 ()
{
  char item1 = 0;
  char* ptr1 = &item1;
  char test1 = 1;

  __atomic_store_with_stshh (ptr1, test1, __ATOMIC_RELAXED, 0);
}

/*
** testFun2:
** ...
**	stshh	keep
**	stlrh	w[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun2 ()
{
  short item2 = 10;
  short* ptr2 = &item2;
  short test2 = 11;
  __atomic_store_with_stshh (ptr2, test2, __ATOMIC_RELEASE, 0);
}

/*
** testFun3:
** ...
**	stshh	strm
**	stlr	w[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun3 ()
{
  unsigned int item3 = 10;
  unsigned int* ptr3 = &item3;
  unsigned int test3 = 11;
  __atomic_store_with_stshh (ptr3, test3, __ATOMIC_SEQ_CST, 1);
}

/*
** testFun4:
** ...
**	stshh	strm
**	str	x[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun4 ()
{
  long item4 = 10;
  long* ptr4 = &item4;
  long test4 = 11;
  __atomic_store_with_stshh (ptr4, test4, __ATOMIC_RELAXED, 1);
}

/*
** testFun5:
** ...
**	stshh	keep
**	stlr	x[0-9]+, \[sp\]
** ...
*/
void
testFun5 ()
{
  long item5 = 10;
  long* ptr5 = &item5;
  long test5item = 11;
  long* test5 = &test5item;
  __atomic_store_with_stshh (ptr5, test5, __ATOMIC_SEQ_CST, 0);
}

/*
** testFun6:
** ...
**	stshh	keep
**	stlr	w[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun6 ()
{
  float item6 = 10;
  float* ptr6 = &item6;
  float test6 = 11;
  __atomic_store_with_stshh (ptr6, test6, __ATOMIC_SEQ_CST, 0);
}

/*
** testFun7:
** ...
**	stshh	strm
**	str	x[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun7 ()
{
  double item7 = 10;
  double* ptr7 = &item7;
  double test7 = 11;
  __atomic_store_with_stshh (ptr7, test7, __ATOMIC_RELAXED, 1);
}

/*
** testFun8:
** ...
**	stshh	keep
**	strb	w[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun8 ()
{
  char item8 = 0;
  char* ptr8 = &item8;
  long test8 = 1;

  __atomic_store_with_stshh (ptr8, test8, __ATOMIC_RELAXED, 0);
}

/*
** testFun9:
** ...
**	stshh	strm
**	str	w[0-9]+, \[x[0-9]+\]
** ...
*/
void
testFun9 ()
{
  int item9 = 0;
  int* ptr9 = &item9;
  float test9 = 1;

  __atomic_store_with_stshh (ptr9, test9, __ATOMIC_RELAXED, 1);
}

/*
** testFun10:
** ...
**	add	(x[0-9]+), \1, 1
**	mov	(w[0-9]+), 7
**	stshh	strm
**	strb	\2, \[\1\]
** ...
*/
static char buf[8];
void
testFun10 (void)
{
  __atomic_store_with_stshh((buf + 1), (char)7, __ATOMIC_RELAXED, 1);
}

/*
** testFun11:
** ...
**	stshh	strm
**	str	wzr, \[x[0-9]+\]
** ...
*/
void
testFun11 ()
{
  int item11 = 10;
  int* ptr11 = &item11;

  __atomic_store_with_stshh (ptr11, 0, __ATOMIC_RELAXED, 1);
}
