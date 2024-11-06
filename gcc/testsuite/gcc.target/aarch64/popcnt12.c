/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nosve"

/*
** foo:
**	cnt	(v[0-9]+\.8b), v0\.8b
**	addv	b0, \1
**	ret
*/
__Uint64x1_t __GIMPLE
foo (__Uint64x1_t x)
{
  __Uint64x1_t z;

  z = .POPCOUNT (x);
  return z;
}
