/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand-details" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* PR middle-end/116508 */

#pragma GCC target "+nocssc"

/*
** h16:
**	sub	w([0-9]+), w0, #1
**	eor	w([0-9]+), w0, w\1
**	and	w([0-9]+), w\1, 65535
**	cmp	w\3, w\2, uxth
**	cset	w0, cc
**	ret
*/

/* when expanding popcount == 1, should use
   `(arg ^ (arg - 1)) > arg - 1` as that has a lower latency
   than doing the popcount then comparing against 1.
   The popcount/addv can be costly. */
unsigned h16 (const unsigned short a) {
	  return __builtin_popcountg (a) == 1;
}

/* unsigned char should also do the same trick */
/* Currently xfailed since the cost does not take into account the
   moving between gprs and vector regs correctly. */
/*
** h8: { xfail *-*-* }
**	sub	w([0-9]+), w0, #1
**	eor	w([0-9]+), w0, w\1
**	and	w([0-9]+), w\1, 255
**	cmp	w\3, w\2, uxtb
**	cset	w0, cc
**	ret
*/


unsigned h8 (const unsigned char a) {
	  return __builtin_popcountg (a) == 1;
}

/* There should be printing out the costs for h8 and h16's popcount == 1 */
/* { dg-final { scan-rtl-dump-times "popcount == 1:" 2 "expand"} } */
