/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

void bar (void);

/* Extern call may change enabled HW features.  */

/*
** foo:
**	...
**	mov	x16, 1
**	...
**	hint	40 // chkfeat x16
**	...
**	bl	bar
**	...
**	mov	x16, 1
**	...
**	hint	40 // chkfeat x16
**	...
*/
unsigned long long
foo (void)
{
  unsigned long long a = __builtin_aarch64_chkfeat (1);
  bar ();
  unsigned long long b = __builtin_aarch64_chkfeat (1);
  return a + b;
}
