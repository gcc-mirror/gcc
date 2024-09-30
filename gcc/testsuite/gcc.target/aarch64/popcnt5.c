/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR target/113042 */

#pragma GCC target "+nocssc"

/*
** h8:
**	ldr	b[0-9]+, \[x0\]
**	cnt	v[0-9]+.8b, v[0-9]+.8b
**	smov	w0, v[0-9]+.b\[0\]
**	ret
*/
/* We should not need the addv here since we only need a byte popcount. */

unsigned h8 (const unsigned char *a) {
	  return __builtin_popcountg (a[0]);
}
