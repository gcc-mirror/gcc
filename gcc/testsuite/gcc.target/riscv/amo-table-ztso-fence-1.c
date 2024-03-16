/* { dg-do compile } */
/* Verify that fence mappings match the Ztso suggested mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	ret
*/
void foo()
{
  __atomic_thread_fence(__ATOMIC_RELAXED);
}
