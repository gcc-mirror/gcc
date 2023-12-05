/* { dg-do compile } */
/* { dg-options "-O -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee_1:
**	mov	p0\.b, p3\.b
**	ret
*/
svcount_t __attribute__ ((noipa))
callee_1 (svcount_t p0, svcount_t p1, svcount_t p2, svcount_t p3)
{
  return p3;
}

/*
** callee_2:
**	str	p0, \[x0\]
**	str	p1, \[x1\]
**	str	p2, \[x2\]
**	str	p3, \[x3\]
**	ret
*/
void __attribute__ ((noipa))
callee_2 (svcount_t p0, svcount_t p1, svcount_t p2, svcount_t p3,
	  svcount_t *ptr0, svcount_t *ptr1, svcount_t *ptr2, svcount_t *ptr3)
{
  *ptr0 = p0;
  *ptr1 = p1;
  *ptr2 = p2;
  *ptr3 = p3;
}

/*
** callee_3:
**	str	p3, \[x0\]
**	ret
*/
void __attribute__ ((noipa))
callee_3 (svbool_t p0, svbool_t p1, svbool_t p2, svcount_t p3, svcount_t *ptr)
{
  *ptr = p3;
}

/*
** callee_4:
**	str	p3, \[x0\]
**	ret
*/
void __attribute__ ((noipa))
callee_4 (svcount_t p0, svcount_t p1, svcount_t p2, svbool_t p3, svbool_t *ptr)
{
  *ptr = p3;
}

/*
** callee_5:
**	ldr	p0, \[x0\]
**	ret
*/
svcount_t __attribute__ ((noipa))
callee_5 (svcount_t p0, svcount_t p1, svcount_t p2, svcount_t p3,
	  svcount_t p4)
{
  return p4;
}

/*
** callee_6:
**	ldr	p0, \[x0\]
**	ret
*/
svcount_t __attribute__ ((noipa))
callee_6 (svcount_t p0, svcount_t p1, svcount_t p2, svcount_t p3,
	  svcount_t p4, int x1, int x2, int x3, int x4, int x5, int x6, int x7,
	  int x8)
{
  return p4;
}

/*
** callee_7:
**	ldr	(x[0-9]+), \[sp\]
**	ldr	p0, \[\1\]
**	ret
*/
svcount_t __attribute__ ((noipa))
callee_7 (svcount_t p0, svcount_t p1, svcount_t p2, svcount_t p3,
	  int x0, int x1, int x2, int x3, int x4, int x5, int x6, int x7,
	  svcount_t p4)
{
  return p4;
}

/*
** caller_1:
**	...
**	ldr	p0, \[x0\]
**	ldr	p1, \[x1\]
**	ldr	p2, \[x2\]
**	ldr	p3, \[x3\]
**	bl	callee_1
**	...
**	str	p0, .*
**	...
*/
void __attribute__ ((noipa))
caller_1 (volatile svcount_t *ptr0, volatile svcount_t *ptr1,
	  volatile svcount_t *ptr2, volatile svcount_t *ptr3,
	  svcount_t *ptr4)
{
  svcount_t p0 = *ptr0;
  svcount_t p1 = *ptr1;
  svcount_t p2 = *ptr2;
  svcount_t p3 = *ptr3;
  *ptr4 = callee_1 (p0, p1, p2, p3);
}

/*
** caller_3:
**	...
**	ldr	p0, \[x1\]
**	ldr	p1, \[x2\]
**	ldr	p2, \[x3\]
**	ldr	p3, \[x4\]
**	bl	callee_3
**	...
*/
void __attribute__ ((noipa))
caller_3 (svcount_t *ptr,
	  volatile svbool_t *ptr0, volatile svbool_t *ptr1,
	  volatile svbool_t *ptr2, volatile svcount_t *ptr3)
{
  svbool_t p0 = *ptr0;
  svbool_t p1 = *ptr1;
  svbool_t p2 = *ptr2;
  svcount_t p3 = *ptr3;
  callee_3 (p0, p1, p2, p3, ptr);
}

/*
** caller_4:
**	...
**	ldr	p0, \[x1\]
**	ldr	p1, \[x2\]
**	ldr	p2, \[x3\]
**	ldr	p3, \[x4\]
**	bl	callee_4
**	...
*/
void __attribute__ ((noipa))
caller_4 (svbool_t *ptr,
	  volatile svcount_t *ptr0, volatile svcount_t *ptr1,
	  volatile svcount_t *ptr2, volatile svbool_t *ptr3)
{
  svcount_t p0 = *ptr0;
  svcount_t p1 = *ptr1;
  svcount_t p2 = *ptr2;
  svbool_t p3 = *ptr3;
  callee_4 (p0, p1, p2, p3, ptr);
}

/*
** caller_5:
**	...
**	ldr	p0, \[x1\]
**	ldr	p1, \[x2\]
**	ldr	p2, \[x3\]
**	ldr	p3, \[x4\]
**	...
**	mov	x0, sp
**	...
**	str	p[0-9]+, \[(?:x0|sp)\]
**	...
**	bl	callee_5
**	...
**	str	p0, .*
**	...
*/
void __attribute__ ((noipa))
caller_5 (svcount_t *ptr,
	  volatile svcount_t *ptr0, volatile svcount_t *ptr1,
	  volatile svcount_t *ptr2, volatile svcount_t *ptr3,
	  volatile svcount_t *ptr4)
{
  svcount_t p0 = *ptr0;
  svcount_t p1 = *ptr1;
  svcount_t p2 = *ptr2;
  svcount_t p3 = *ptr3;
  svcount_t p4 = *ptr4;
  *ptr = callee_5 (p0, p1, p2, p3, p4);
}

/*
** caller_7:
**	...
**	ldr	(p[0-9]+), \[x2\]
**	...
**	str	\1, \[(x[0-9]+)\]
**	...
**	str	\2, \[sp\]
**	...
**	bl	callee_7
**	...
*/
void __attribute__ ((noipa))
caller_7 (svcount_t *ptr, volatile svcount_t *ptr0, volatile svcount_t *ptr1)
{
  svcount_t p0 = *ptr0;
  svcount_t p1 = *ptr1;
  *ptr = callee_7 (p0, p0, p0, p0, 0, 0, 0, 0, 0, 0, 0, 0, p1);
}
