/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR target/113042 */

#pragma GCC target "+cssc"

/*
** h128:
**	ldp	x([0-9]+), x([0-9]+), \[x0\]
**	cnt	x([0-9]+), x([0-9]+)
**	cnt	x([0-9]+), x([0-9]+)
**	add	w0, w([0-9]+), w([0-9]+)
**	ret
*/


unsigned h128 (const unsigned __int128 *a) {
  return __builtin_popcountg (a[0]);
}

/* popcount with CSSC should be split into 2 sections. */
/* { dg-final { scan-tree-dump-not "POPCOUNT " "optimized" } } */
/* { dg-final { scan-tree-dump-times " __builtin_popcount" 2 "optimized" } } */

