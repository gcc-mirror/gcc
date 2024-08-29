/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "POPCOUNT \\\(" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_popcount \\\(" "optimized" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR middle-end/90693 */

#pragma GCC target "+nocssc"

/*
** le32:
**	sub	w([0-9]+), w0, #1
**	tst	w\1, w0
**	cset	w0, eq
**	ret
*/

unsigned le32 (const unsigned int a) {
  return __builtin_popcountg (a) <= 1;
}

/*
** gt32:
**	sub	w([0-9]+), w0, #1
**	tst	w\1, w0
**	cset	w0, ne
**	ret
*/
unsigned gt32 (const unsigned int a) {
  return __builtin_popcountg (a) > 1;
}
