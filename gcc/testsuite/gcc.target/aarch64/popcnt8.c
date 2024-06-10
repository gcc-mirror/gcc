/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR target/113042 */

#pragma GCC target "+cssc"

/*
** h8:
**	ldrh	w[0-9]+, \[x0\]
**	cnt	w[0-9]+, w[0-9]+
**	ret
*/
/* We should not produce any extra zero extend for this code */

unsigned h8 (const unsigned short *a) {
	  return __builtin_popcountg (a[0]);
}
