/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** func_init4:
**	mov	z0\.d, x1
**	insr	z0\.d, x0
**	ret
*/
svint64_t __attribute__ ((noipa))
func_init4 (int64_t a, int64_t b)
{
  svint64_t temp = { a, b };
  return temp;
}

/*
** func_init3:
**	fmov	s0, w2
**	insr	z0\.s, w1
**	insr	z0\.s, w0
**	ret
*/
svint32_t __attribute__ ((noipa))
func_init3 (int32_t a, int32_t b, int32_t c)
{
  svint32_t temp = { a, b, c };
  return temp;
}
