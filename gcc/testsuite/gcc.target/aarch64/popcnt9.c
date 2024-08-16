/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR target/113042 */

#pragma GCC target "+nocssc"

/*
** h128:
**	ldr	q([0-9]+), \[x0\]
**	cnt	v([0-9]+).16b, v\1.16b
**	addv	b([0-9]+), v\2.16b
**	fmov	w0, s\3
**	ret
*/


unsigned h128 (const unsigned __int128 *a) {
	  return __builtin_popcountg (a[0]);
}

/* There should be only one POPCOUNT. */
/* { dg-final { scan-tree-dump-times "POPCOUNT " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " __builtin_popcount"  "optimized" } } */

