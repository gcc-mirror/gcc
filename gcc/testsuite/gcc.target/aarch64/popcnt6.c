/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR target/113042 */

#pragma GCC target "+nocssc"

/*
** h8:
**	ldr	h[0-9]+, \[x0\]
**	cnt	v[0-9]+.8b, v[0-9]+.8b
**	addv	b[0-9]+, v[0-9]+.8b
**	fmov	w0, s[0-9]+
**	ret
*/

unsigned h8 (const unsigned short *a) {
	  return __builtin_popcountg (a[0]);
}
