/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzvector" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler-not {\tvperm\t} } } */

#include <vecintrin.h>

/*
** test_z13_rr:
**	verllh	%v24,%v24,8
**	br	%r14
*/
vector unsigned short
test_z13_rr (vector unsigned short a)
{
  return vec_revb (a);
}

/*
** test_z15_rr:
**	verllh	%v24,%v24,8
**	br	%r14
*/
__attribute__ ((target ("arch=z15")))
vector unsigned short
test_z15_rr (vector unsigned short a)
{
  return vec_revb (a);
}

/*
** test_z13_rm:
**	vl	%v24,0\(%r2\),3
**	verllh	%v24,%v24,8
**	br	%r14
*/
vector unsigned short
test_z13_rm (vector unsigned short *a)
{
  return vec_revb (*a);
}

/*
** test_z15_rm:
**	vlbrh	%v24,0\(%r2\)
**	br	%r14
*/
__attribute__ ((target ("arch=z15")))
vector unsigned short
test_z15_rm (vector unsigned short *a)
{
  return vec_revb (*a);
}

/*
** test_z13_mr:
**	verllh	%v24,%v24,8
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/
void
test_z13_mr (vector unsigned short *dst, vector unsigned short a)
{
  *dst = vec_revb (a);
}

/*
** test_z15_mr:
**	vstbrh	%v24,0\(%r2\)
**	br	%r14
*/
__attribute__ ((target ("arch=z15")))
void
test_z15_mr (vector unsigned short *dst, vector unsigned short a)
{
  *dst = vec_revb (a);
}
