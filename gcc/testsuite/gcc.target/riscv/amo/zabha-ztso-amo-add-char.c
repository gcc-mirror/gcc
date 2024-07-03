/* Verify that atomic op mappings match the PSABI doc's recommended mapping.  */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-add-options riscv_zabha } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_add_fetch_char_relaxed:
**	amoadd\.b\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_char_relaxed (char* bar, char baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_add_fetch_char_acquire:
**	amoadd\.b\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_char_acquire (char* bar, char baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_add_fetch_char_release:
**	amoadd\.b\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_char_release (char* bar, char baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELEASE);
}

/*
** atomic_add_fetch_char_acq_rel:
**	amoadd\.b\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_char_acq_rel (char* bar, char baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQ_REL);
}

/*
** atomic_add_fetch_char_seq_cst:
**	amoadd\.b\tzero,a1,0\(a0\)
**	ret
*/
void atomic_add_fetch_char_seq_cst (char* bar, char baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_SEQ_CST);
}
