/* { dg-do compile } */
/* Verify that load mappings match Table A.6's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	fence\trw,rw
**	lw\ta[0-9]+,0\(a0\)
**	fence\tr,rw
**	sw\ta[0-9]+,0\(a1\)
**	ret
*/
void foo (int* bar, int* baz)
{
  __atomic_load(bar, baz, __ATOMIC_SEQ_CST);
}
