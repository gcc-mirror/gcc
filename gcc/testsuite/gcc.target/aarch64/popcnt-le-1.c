/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand-details" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR middle-end/90693 */

#pragma GCC target "+nocssc"

/*
** le32:
**	sub	w([0-9]+), w0, #1
**	tst	(?:w0, w\1|w\1, w0)
**	cset	w0, eq
**	ret
*/

unsigned le32 (const unsigned int a) {
  return __builtin_popcountg (a) <= 1;
}

/*
** gt32:
**	sub	w([0-9]+), w0, #1
**	tst	(?:w0, w\1|w\1, w0)
**	cset	w0, ne
**	ret
*/
unsigned gt32 (const unsigned int a) {
  return __builtin_popcountg (a) > 1;
}
