/*
** Verify that atomic op mappings match the PSABI doc's recommended mapping.
** compare_exchange ops with seq_cst failure ordering need a leading fence
** to remain compatible with Table A.6 (A6C).
*/
/* { dg-do compile } */
/* { dg-options "-O3 -std=c++17" } */
/* { dg-add-options riscv_zacas } */
/* { dg-add-options riscv_ztso } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_compare_exchange_weak_int_relaxed_seq_cst:
**	...
**	fence\trw,rw
**	amoadd\.w\t[atx][0-9]+,a2,0\(a0\)
**	...
*/
void atomic_compare_exchange_weak_int_relaxed_seq_cst (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_RELAXED, __ATOMIC_SEQ_CST);
}

/*
** atomic_compare_exchange_weak_int_seq_cst_seq_cst:
**	...
**	fence\trw,rw
**	amoadd\.w\t[atx][0-9]+,a2,0\(a0\)
**	...
*/
void atomic_compare_exchange_weak_int_seq_cst_seq_cst (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

/*
** atomic_compare_exchange_strong_int_relaxed_seq_cst:
**	...
**	fence\trw,rw
**	amoadd\.w\t[atx][0-9]+,a2,0\(a0\)
**	...
*/
void atomic_compare_exchange_strong_int_relaxed_seq_cst (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_RELAXED, __ATOMIC_SEQ_CST);
}

/*
** atomic_compare_exchange_strong_int_seq_cst_seq_cst:
**	...
**	fence\trw,rw
**	amoadd\.w\t[atx][0-9]+,a2,0\(a0\)
**	...
*/
void atomic_compare_exchange_strong_int_seq_cst_seq_cst (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
