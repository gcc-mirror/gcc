/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=none" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
**foo1:
**	mov	x16, 1
**	hint	40 // chkfeat x16
**	mov	x0, x16
**	ret
*/
unsigned long long
foo1 (void)
{
  return __builtin_aarch64_chkfeat (1);
}

/*
**foo2:
**	mov	x16, 1
**	movk	x16, 0x5678, lsl 32
**	movk	x16, 0x1234, lsl 48
**	hint	40 // chkfeat x16
**	mov	x0, x16
**	ret
*/
unsigned long long
foo2 (void)
{
  return __builtin_aarch64_chkfeat (0x1234567800000001);
}

/*
**foo3:
**	mov	x16, x0
**	hint	40 // chkfeat x16
**	mov	x0, x16
**	ret
*/
unsigned long long
foo3 (unsigned long long x)
{
  return __builtin_aarch64_chkfeat (x);
}

/*
**foo4:
**	ldr	x16, \[x0\]
**	hint	40 // chkfeat x16
**	str	x16, \[x0\]
**	ret
*/
void
foo4 (unsigned long long *p)
{
  *p = __builtin_aarch64_chkfeat (*p);
}

/*
**foo5:
**	mov	x16, 1
**	hint	40 // chkfeat x16
**	cmp	x16, 0
**(
**	csel	w0, w1, w0, eq
**|
**	csel	w0, w0, w1, ne
**)
**	ret
*/
int
foo5 (int x, int y)
{
  return __builtin_aarch64_chkfeat (1) ? x : y;
}
