/* { dg-do compile } */
/* Verify that load mappings match the PSABI doc's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-remove-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_load_long_seq_cst:
**	fence\trw,rw
**	l[wd]\t[atx][0-9]+,0\(a0\)
**	fence\tr,rw
**	s[wd]\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_long_seq_cst (long* bar, long* baz)
{
  __atomic_load(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_load_int_seq_cst:
**	fence\trw,rw
**	lw\t[atx][0-9]+,0\(a0\)
**	fence\tr,rw
**	sw\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_int_seq_cst (int* bar, int* baz)
{
  __atomic_load(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_load_short_seq_cst:
**	fence\trw,rw
**	lh\t[atx][0-9]+,0\(a0\)
**	fence\tr,rw
**	sh\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_short_seq_cst (short* bar, short* baz)
{
  __atomic_load(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_load_char_seq_cst:
**	fence\trw,rw
**	lb\t[atx][0-9]+,0\(a0\)
**	fence\tr,rw
**	sb\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_char_seq_cst (char* bar, char* baz)
{
  __atomic_load(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_load_bool_seq_cst:
**	fence\trw,rw
**	lb\t[atx][0-9]+,0\(a0\)
**	fence\tr,rw
**	sb\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_bool_seq_cst (_Bool* bar, _Bool* baz)
{
  __atomic_load(bar, baz, __ATOMIC_SEQ_CST);
}
