/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=none" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
**foo1:
**	mrs	x0, s3_3_c2_c5_1 // gcspr_el0
**	ret
*/
void *
foo1 (void)
{
  return __builtin_aarch64_gcspr ();
}

/*
**foo2:
**	mrs	x[0-9]*, s3_3_c2_c5_1 // gcspr_el0
**	sysl	xzr, #3, c7, c7, #1 // gcspopm
**	mrs	x[0-9]*, s3_3_c2_c5_1 // gcspr_el0
**	sub	x0, x[0-9]*, x[0-9]*
**	ret
*/
long
foo2 (void)
{
  const char *p = __builtin_aarch64_gcspr ();
  __builtin_aarch64_gcspopm ();
  const char *q = __builtin_aarch64_gcspr ();
  return p - q;
}
