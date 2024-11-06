/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** f1:
**	cntd	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f1 (int x)
{
  if (x & 1)
    __builtin_unreachable ();
  x /= 2;
  return x * svcntw();
}

/*
** f2:
**	cntd	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f2 (int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x /= 4;
  return x * svcnth();
}

/*
** f3:
**	cntd	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f3 (int x)
{
  if (x & 7)
    __builtin_unreachable ();
  x /= 8;
  return x * svcntb();
}

/*
** f4:
**	cntw	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f4 (int x)
{
  if (x & 1)
    __builtin_unreachable ();
  x /= 2;
  return x * svcnth();
}

/*
** f5:
**	cntw	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f5 (int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x /= 4;
  return x * svcntb();
}

/*
** f6:
**	cnth	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f6 (int x)
{
  if (x & 1)
    __builtin_unreachable ();
  x /= 2;
  return x * svcntb();
}

/*
** f7:
**	cntb	x([0-9]+)
**	mul	w0, (w0, w\1|w\1, w0)
**	ret
*/
int
f7 (int x)
{
  if (x & 15)
    __builtin_unreachable ();
  x /= 16;
  return x * svcntb() * 16;
}
