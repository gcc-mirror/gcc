/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=none" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
**foo1:
**	sys	#3, c7, c7, #2, x0 // gcsss1
**	mov	(x[0-9]*), 0
**	sysl	\1, #3, c7, c7, #3 // gcsss2
**	ret
*/
void
foo1 (void *p)
{
  __builtin_aarch64_gcsss (p);
}

/*
**foo2:
**	sys	#3, c7, c7, #2, x0 // gcsss1
**	mov	x0, 0
**	sysl	x0, #3, c7, c7, #3 // gcsss2
**	ret
*/
void *
foo2 (void *p)
{
  return __builtin_aarch64_gcsss (p);
}

/*
**foo3:
**	mov	x16, 1
**	hint	40 // chkfeat x16
**	cbz	x16, .*
**	ret
**	sys	#3, c7, c7, #2, x0 // gcsss1
**	mov	x0, (0|x16)
**	sysl	x0, #3, c7, c7, #3 // gcsss2
**	ret
*/
void *
foo3 (void *p)
{
  if (__builtin_aarch64_chkfeat (1) == 0)
    return __builtin_aarch64_gcsss (p);
  return p;
}
