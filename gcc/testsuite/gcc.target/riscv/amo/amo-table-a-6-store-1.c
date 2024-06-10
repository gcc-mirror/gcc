/* { dg-do compile } */
/* Verify that store mappings match Table A.6's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	lw\ta[0-9]+,0\(a1\)
**	sw\ta[0-9]+,0\(a0\)
**	ret
*/
void foo (int* bar, int* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}
