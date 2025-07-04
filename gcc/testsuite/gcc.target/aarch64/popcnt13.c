/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC target "+nocssc+sve"

/*
** h128:
**	ldr	q([0-9]+), \[x0\]
**	ptrue	p([0-9]+).b, vl16
**	cnt	z([0-9]+).d, p\2/m, z\1.d
**	addp	d([0-9]+), v\3.2d
**	fmov	x0, d\4
**	ret
*/

unsigned h128 (const unsigned __int128 *a) {
	  return __builtin_popcountg (a[0]);
}

/* There should be only one POPCOUNT. */
/* { dg-final { scan-tree-dump-times "POPCOUNT " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " __builtin_popcount"  "optimized" } } */

