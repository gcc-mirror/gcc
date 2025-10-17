/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** foo:
**	ptrue	p0\.b, all
**	brkb	p0\.b, p0/z, p0\.b
**	ret
*/
svbool_t foo () {
  return svbrkb_b_m (svpfalse (), svptrue_b8 (), svptrue_b8 ());
}

/*
** bar:
**	ptrue	p0\.b, all
**	brka	p0\.b, p0/z, p0\.b
**	ret
*/
svbool_t bar () {
  return svbrka_b_m (svpfalse (), svptrue_b8 (), svptrue_b8 ());
}
