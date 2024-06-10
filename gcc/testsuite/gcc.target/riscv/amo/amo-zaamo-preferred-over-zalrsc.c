/* { dg-do compile } */
/* Ensure that AMO ops are emitted when both zalrsc and zaamo are enabled.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zalrsc } */
/* { dg-add-options riscv_zaamo } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	amoadd\.w\tzero,a1,0\(a0\)
**	ret
*/
void foo (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELAXED);
}
