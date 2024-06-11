/* { dg-do compile } */
/* Verify that store mappings match the Ztso suggested mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	lw\t[atx][0-9]+,0\(a1\)
**	sw\t[atx][0-9]+,0\(a0\)
**	ret
*/
void foo (int* bar, int* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}
