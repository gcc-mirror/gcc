/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** b_lt_257:
**	mov	w0, 1
**	ret
*/
int
b_lt_257 (void)
{
  unsigned int vl = svcntb ();

  return vl < 257;
}

/*
** h_ge_8:
**	mov	w0, 1
**	ret
*/
int
h_ge_8 (void)
{
  return svcnth () >= 8;
}

/*
** w_le_64:
**	mov	w0, 1
**	ret
*/
int
w_le_64 (void)
{
  return svcntw () <= 64;
}

/*
** d_gt_1:
**	mov	w0, 1
**	ret
*/
int
d_gt_1 (void)
{
  return svcntd () > 1;
}

/*
** b_ne_0:
**	mov	w0, 1
**	ret
*/
int
b_ne_0 (void)
{
  return svcntb () != 0;
}

/*
** b_le_15:
**	mov	w0, 0
**	ret
*/
int
b_le_15 (void)
{
  return svcntb () <= 15;
}

/*
** h_lt_8:
**	mov	w0, 0
**	ret
*/
int
h_lt_8 (void)
{
  return svcnth () < 8;
}

/*
** w_gt_64:
**	mov	w0, 0
**	ret
*/
int
w_gt_64 (void)
{
  return svcntw () > 64;
}

/*
** d_eq_0:
**	mov	w0, 0
**	ret
*/
int
d_eq_0 (void)
{
  return svcntd () == 0;
}

/*
** b_lt_256:
**	cntb	x0
**	cmp	x0, 256
**	cset	w0, cc
**	ret
*/
int
b_lt_256 (void)
{
  return svcntb () < 256;
}

/*
** w_gt_4:
**	cntw	x0
**	cmp	x0, 4
**	cset	w0, hi
**	ret
*/
int
w_gt_4 (void)
{
  return svcntw () > 4;
}

/*
** b_pat_all_lt_257:
**	mov	w0, 1
**	ret
*/
int
b_pat_all_lt_257 (void)
{
  return svcntb_pat (SV_ALL) < 257;
}

/*
** w_pat_all_le_64:
**	mov	w0, 1
**	ret
*/
int
w_pat_all_le_64 (void)
{
  return svcntw_pat (SV_ALL) <= 64;
}
