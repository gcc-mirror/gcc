/* Verify that atomic op mappings match the PSABI doc's recommended mapping.  */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zabha } */
/* { dg-remove-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_add_fetch_short_relaxed:
**	amoadd\.h\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_short_relaxed (short* bar, short baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_add_fetch_short_acquire:
**	amoadd\.h\.aq\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_short_acquire (short* bar, short baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_add_fetch_short_release:
**	amoadd\.h\.rl\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_short_release (short* bar, short baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELEASE);
}

/*
** atomic_add_fetch_short_acq_rel:
**	amoadd\.h\.aqrl\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_short_acq_rel (short* bar, short baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQ_REL);
}

/*
** atomic_add_fetch_short_seq_cst:
**	amoadd\.h\.aqrl\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_short_seq_cst (short* bar, short baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_SEQ_CST);
}
