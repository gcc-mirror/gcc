/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.1-a -save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

/*
** testFun1:
** ...
**	shuh
**	ldaddb	(w[0-9]+), \1, \[x[0-9]+\]
** ...
*/
char
testFun1 ()
{
  char item1 = 0;
  char* ptr1 = &item1;
  char test1 = 1;

  return __arm_atomic_fetch_add_with_hint (ptr1, test1, __ATOMIC_RELAXED, 0);
}

/*
** testFun2:
** ...
**	shuh
**	ldaddlh	(w[0-9]+), \1, \[x[0-9]+\]
** ...
*/
short
testFun2 ()
{
  short item2 = 10;
  short* ptr2 = &item2;
  short test2 = 11;
  return __arm_atomic_fetch_add_with_hint (ptr2, test2, __ATOMIC_RELEASE, 0);
}

/*
** testFun3:
** ...
**	shuh	ph
**	ldaddal	(w[0-9]+), \1, \[x[0-9]+\]
** ...
*/
int
testFun3 ()
{
  unsigned int item3 = 10;
  unsigned int* ptr3 = &item3;
  unsigned int test3 = 11;
  return __arm_atomic_fetch_add_with_hint (ptr3, test3, __ATOMIC_SEQ_CST, 1);
}

/*
** testFun4:
** ...
**	shuh	ph
**	ldadd	(x[0-9]+), \1, \[x[0-9]+\]
** ...
*/
void
testFun4 ()
{
  long item4 = 10;
  long* ptr4 = &item4;
  long test4 = 11;
  __arm_atomic_fetch_add_with_hint (ptr4, test4, __ATOMIC_RELAXED, 1);
}

/*
** testFun5:
** ...
**	shuh
**	ldaddal	(x[0-9]+), \1, \[x[0-9]+\]
** ...
*/
void
testFun5 ()
{
  long item5 = 10;
  long *ptritem = &item5;
  long **ptr5 = &ptritem;
  long test5item = 11;
  long *test5 = &test5item;
  __arm_atomic_fetch_add_with_hint (ptr5, test5, __ATOMIC_SEQ_CST, 0);
}

/*
** testFun6:
** ...
**	shuh
**	ldaddb	(w[0-9]+), \1, \[x[0-9]+\]
** ...
*/
void
testFun6 ()
{
  char item8 = 0;
  char* ptr8 = &item8;
  long test8 = 1;

  __arm_atomic_fetch_add_with_hint (ptr8, test8, __ATOMIC_RELAXED, 0);
}

/*
** testFun7:
** ...
**	add	(x[0-9]+), \1, 1
**	mov	(w[0-9]+), 7
**	shuh	ph
**	ldaddb	\2, \2, \[x[0-9]+\]
** ...
*/
static char buf[8];
void
testFun7 (void)
{
  __arm_atomic_fetch_add_with_hint((buf + 1), (char)7, __ATOMIC_RELAXED, 1);
}


/*
** signed_fetch_add_hint:
** ...
**	shuh	ph
**	ldadd	(w[0-9]+), \1, \[x[0-9]+\]
**	sxtw	x[0-9]+, \1
** ...
*/
long
signed_fetch_add_hint (int x, int *p)
{
  return __arm_atomic_fetch_add_with_hint (p, x, __ATOMIC_RELAXED, 1);
}


/*
** unsigned_fetch_add_hint:
** ...
**	shuh	ph
**	ldadd	(w[0-9]+), \1, \[x[0-9]+\]
**	uxtw	x[0-9]+, \1
** ...
*/
long
unsigned_fetch_add_hint (unsigned int x, unsigned int *p)
{
  return __arm_atomic_fetch_add_with_hint (p, x, __ATOMIC_RELAXED, 1);
}