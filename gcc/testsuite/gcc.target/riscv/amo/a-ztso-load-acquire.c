/* { dg-do compile } */
/* Verify that load mappings match the PSABI doc's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** atomic_load_long_acquire:
**	l[wd]\t[atx][0-9]+,0\(a0\)
**	s[wd]\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_long_acquire (long* bar, long* baz)
{
  __atomic_load(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_load_int_acquire:
**	lw\t[atx][0-9]+,0\(a0\)
**	sw\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_int_acquire (int* bar, int* baz)
{
  __atomic_load(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_load_short_acquire:
**	lh\t[atx][0-9]+,0\(a0\)
**	sh\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_short_acquire (short* bar, short* baz)
{
  __atomic_load(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_load_char_acquire:
**	lb\t[atx][0-9]+,0\(a0\)
**	sb\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_char_acquire (char* bar, char* baz)
{
  __atomic_load(bar, baz, __ATOMIC_ACQUIRE);
}

/*
** atomic_load_bool_acquire:
**	lb\t[atx][0-9]+,0\(a0\)
**	sb\t[atx][0-9]+,0\(a1\)
**	ret
*/
void atomic_load_bool_acquire (_Bool* bar, _Bool* baz)
{
  __atomic_load(bar, baz, __ATOMIC_ACQUIRE);
}
