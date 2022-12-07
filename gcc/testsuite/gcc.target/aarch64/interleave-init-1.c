/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** foo:
**	...
**	dup	v[0-9]+\.8h, w[0-9]+
**	dup	v[0-9]+\.8h, w[0-9]+
**	zip1	v[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h
**	...
**	ret
*/

int16x8_t foo(int16_t x, int y)
{
  int16x8_t v = (int16x8_t) {x, y, x, y, x, y, x, y}; 
  return v;
}

/*
** foo2:
**	...
**	dup	v[0-9]+\.8h, w[0-9]+
**	movi	v[0-9]+\.8h, 0x1
**	zip1	v[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h
**	...
**	ret
*/

int16x8_t foo2(int16_t x) 
{
  int16x8_t v = (int16x8_t) {x, 1, x, 1, x, 1, x, 1}; 
  return v;
}
