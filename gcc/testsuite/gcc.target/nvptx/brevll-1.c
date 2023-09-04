/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies {**} {} } } */

unsigned long foo(unsigned long x)
{
  return __builtin_nvptx_brevll(x);
}
/*
** foo:
**	...
**	mov\.u64	(%r[0-9]+), %ar0;
**	brev\.b64	%value, \1;
**	st\.param\.u64	\[%value_out\], %value;
**	ret;
*/
