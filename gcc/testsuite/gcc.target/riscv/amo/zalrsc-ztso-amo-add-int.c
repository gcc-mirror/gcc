/* { dg-do compile } */
/* Verify that lrsc atomic op mappings match the PSABI doc's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zalrsc } */
/* { dg-add-options riscv_ztso } */
/* { dg-remove-options riscv_zaamo } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_add_fetch_int_relaxed:
**	1:
**	lr.w\t[atx][0-9]+, 0\(a0\)
**	add\t[atx][0-9]+, [atx][0-9]+, a1
**	sc.w\t[atx][0-9]+, [atx][0-9]+, 0\(a0\)
**      bnez\t[atx][0-9]+, 1b
**	ret
*/
void atomic_add_fetch_int_relaxed (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_add_fetch_int_acquire:
**	1:
**	lr.w\t[atx][0-9]+, 0\(a0\)
**	add\t[atx][0-9]+, [atx][0-9]+, a1
**	sc.w\t[atx][0-9]+, [atx][0-9]+, 0\(a0\)
**      bnez\t[atx][0-9]+, 1b
**	ret
*/
void atomic_add_fetch_int_acquire (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_add_fetch_int_release:
**	1:
**	lr.w\t[atx][0-9]+, 0\(a0\)
**	add\t[atx][0-9]+, [atx][0-9]+, a1
**	sc.w\t[atx][0-9]+, [atx][0-9]+, 0\(a0\)
**      bnez\t[atx][0-9]+, 1b
**	ret
*/
void atomic_add_fetch_int_release (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_RELEASE);
}

/*
** atomic_add_fetch_int_acq_rel:
**	1:
**	lr.w\t[atx][0-9]+, 0\(a0\)
**	add\t[atx][0-9]+, [atx][0-9]+, a1
**	sc.w\t[atx][0-9]+, [atx][0-9]+, 0\(a0\)
**      bnez\t[atx][0-9]+, 1b
**	ret
*/
void atomic_add_fetch_int_acq_rel (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_ACQ_REL);
}

/*
** atomic_add_fetch_int_seq_cst:
**	1:
**	lr.w.aqrl\t[atx][0-9]+, 0\(a0\)
**	add\t[atx][0-9]+, [atx][0-9]+, a1
**	sc.w.rl\t[atx][0-9]+, [atx][0-9]+, 0\(a0\)
**      bnez\t[atx][0-9]+, 1b
**	ret
*/
void atomic_add_fetch_int_seq_cst (int* bar, int baz)
{
  __atomic_add_fetch(bar, baz, __ATOMIC_SEQ_CST);
}
