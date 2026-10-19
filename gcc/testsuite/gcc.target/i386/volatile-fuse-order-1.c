/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-pic -masm=att" } */
/* { dg-final { check-function-bodies "**" "" "" { target { ! *-*-darwin* } } } } */

/* The two volatile reads are in separate full expressions, so x must be
   read before y.  Combine must not put both reads into one compare insn,
   which gives them no order.  */

/*
**f:
**...
**	movl	x(|\(%rip\)), %eax
**	cmpl	y(|\(%rip\)), %eax
**...
*/

extern volatile int x, y;

int
f (void)
{
  int a = x;
  int b = y;
  return a < b;
}
