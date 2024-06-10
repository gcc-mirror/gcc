/* { dg-do compile } */
/* Verify that lrsc atomic op mappings match Table A.6's recommended mapping.  */
/* { dg-options "-O3 -march=rv64id_zalrsc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	1:
**	lr.w.aqrl\t[atx][0-9]+, 0\(a0\)
**	add\t[atx][0-9]+, [atx][0-9]+, a1
**	sc.w.rl\t[atx][0-9]+, [atx][0-9]+, 0\(a0\)
**      bnez\t[atx][0-9]+, 1b
**	ret
*/
void foo (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_SEQ_CST);
}
