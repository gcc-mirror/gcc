/* { dg-do compile } */
/* Verify that load mappings match the PSABI doc's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-remove-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_load_long_relaxed:
**	l[wd]\t[atx][0-9]+,0\(a0\)
**	s[wd]\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_long_relaxed (long* bar, long* baz)
{
  __atomic_load(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_load_int_relaxed:
**	lw\t[atx][0-9]+,0\(a0\)
**	sw\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_int_relaxed (int* bar, int* baz)
{
  __atomic_load(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_load_short_relaxed:
**	lh\t[atx][0-9]+,0\(a0\)
**	sh\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_short_relaxed (short* bar, short* baz)
{
  __atomic_load(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_load_char_relaxed:
**	lb\t[atx][0-9]+,0\(a0\)
**	sb\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_char_relaxed (char* bar, char* baz)
{
  __atomic_load(bar, baz, __ATOMIC_RELAXED);
}

/*
** atomic_load_bool_relaxed:
**	lb\t[atx][0-9]+,0\(a0\)
**	sb\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_bool_relaxed (_Bool* bar, _Bool* baz)
{
  __atomic_load(bar, baz, __ATOMIC_RELAXED);
}
