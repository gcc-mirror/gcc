/* { dg-do compile } */
/* Verify that store mappings match the PSABI doc's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_store_long_relaxed:
**	l[wd]\t[atx][0-9]+,0\(a1\)
**	s[wd]\t[atx][0-9]+,0\(a0\)
**	ret
*/
void atomic_store_long_relaxed (long* bar, long* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_store_int_relaxed:
**	lw\t[atx][0-9]+,0\(a1\)
**	sw\t[atx][0-9]+,0\(a0\)
**	ret
*/
void atomic_store_int_relaxed (int* bar, int* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_store_short_relaxed:
**	lhu\t[atx][0-9]+,0\(a1\)
**	sh\t[atx][0-9]+,0\(a0\)
**	ret
*/
void atomic_store_short_relaxed (short* bar, short* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_store_char_relaxed:
**	lbu\t[atx][0-9]+,0\(a1\)
**	sb\t[atx][0-9]+,0\(a0\)
**	ret
*/
void atomic_store_char_relaxed (char* bar, char* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_store_bool_relaxed:
**	lbu\t[atx][0-9]+,0\(a1\)
**	sb\t[atx][0-9]+,0\(a0\)
**	ret
*/
void atomic_store_bool_relaxed (_Bool* bar, _Bool* baz)
{
  __atomic_store(bar, baz, __ATOMIC_RELAXED);
}
