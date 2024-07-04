/* { dg-do compile } */
/* Verify that store mappings match the PSABI doc's recommended compatibility
   mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_store_long_seq_cst:
**	l[wd]\t[atx][0-9]+,0\(a1\)
**	s[wd]\t[atx][0-9]+,0\(a0\)
**	fence\trw,rw
**	ret
*/
void atomic_store_long_seq_cst (long* bar, long* baz)
{
  __atomic_store(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_store_int_seq_cst:
**	lw\t[atx][0-9]+,0\(a1\)
**	sw\t[atx][0-9]+,0\(a0\)
**	fence\trw,rw
**	ret
*/
void atomic_store_int_seq_cst (int* bar, int* baz)
{
  __atomic_store(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_store_short_seq_cst:
**	lhu\t[atx][0-9]+,0\(a1\)
**	sh\t[atx][0-9]+,0\(a0\)
**	fence\trw,rw
**	ret
*/
void atomic_store_short_seq_cst (short* bar, short* baz)
{
  __atomic_store(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_store_char_seq_cst:
**	lbu\t[atx][0-9]+,0\(a1\)
**	sb\t[atx][0-9]+,0\(a0\)
**	fence\trw,rw
**	ret
*/
void atomic_store_char_seq_cst (char* bar, char* baz)
{
  __atomic_store(bar, baz, __ATOMIC_SEQ_CST);
}

/*
** atomic_store_bool_seq_cst:
**	lbu\t[atx][0-9]+,0\(a1\)
**	sb\t[atx][0-9]+,0\(a0\)
**	fence\trw,rw
**	ret
*/
void atomic_store_bool_seq_cst (_Bool* bar, _Bool* baz)
{
  __atomic_store(bar, baz, __ATOMIC_SEQ_CST);
}
