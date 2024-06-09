/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies {**} {} } } */

unsigned int foo(unsigned int x)
{
  return __builtin_nvptx_brev(x);
}
/*
** foo:
**	...
**	mov\.u32	(%r[0-9]+), %ar0;
**	brev\.b32	%value, \1;
**	st\.param\.u32	\[%value_out\], %value;
**	ret;
*/
