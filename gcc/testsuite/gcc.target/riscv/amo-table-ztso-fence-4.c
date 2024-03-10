/* { dg-do compile } */
/* Verify that fence mappings match the Ztso suggested mapping.  */
/* { dg-options "-march=rv64id_ztso -mabi=lp64d -O3" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	ret
*/
void foo()
{
  __atomic_thread_fence(__ATOMIC_ACQ_REL);
}
