/* { dg-do compile } */
/* { dg-options "-O2 -marm" } */
/* { dg-require-effective-target arm_arch_v7ve_neon_ok } */
/* { dg-add-options arm_arch_v7ve_neon } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	add	r0, r0, r1
**	ldrd	r0, r1, \[r0\]
**	bx	lr
*/
long long f1 (char *p, int i)
{
  return __atomic_load_n ((long long *)(p + i), __ATOMIC_RELAXED);
}

