 /* Test the ACLE hint intrinsics effect on memory.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

/*
** foo:
** ...
**	mov	w[0-9]+, 5
**	str	w[0-9]+, \[x0\]
**	yield
**	mov	w[0-9]+, 4
**	str	w[0-9]+, \[x0\]
**	wfe
**	mov	w[0-9]+, 3
**	str	w[0-9]+, \[x0\]
**	sev
**	str	w[0-9]+, \[x0\]
**	sevl
**	mov	w[0-9]+, 9
**	str	w[0-9]+, \[x0\]
**	wfi
**	mov	w0, 0
**	ret
*/
int foo (int* counter)
{
  *counter = 5;
  __yield();
  *counter = 4;
  __wfe();
  *counter = 3;
  __sev();
  *counter = 4;
  __sevl();
  *counter = 9;
  __wfi();
  return 0;
}

/*
** foo1:
** ...
**	mov	w[0-9]+, 5
**	str	w[0-9]+, \[x0\]
**	yield
**	mov	w[0-9]+, 4
**	str	w[0-9]+, \[x0\]
**	yield
**	mov	w[0-9]+, 3
**	str	w[0-9]+, \[x0\]
**	yield
**	mov	w[0-9]+, 6
**	str	w[0-9]+, \[x0\]
**	yield
**	mov	w[0-9]+, 9
**	str	w[0-9]+, \[x0\]
**	yield
**	mov	w0, 0
**	ret
*/
int foo1 (int* counter)
{
  *counter = 5;
  __yield();
  *counter = 4;
  __yield();
  *counter = 3;
  __yield();
  *counter = 6;
  __yield();
  *counter = 9;
  __yield();
  return 0;
}
