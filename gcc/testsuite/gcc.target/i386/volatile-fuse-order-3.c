/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-pic -masm=att" } */
/* { dg-final { check-function-bodies "**" "" "" { target { ! *-*-darwin* } } } } */

/* As volatile-fuse-order-1.c, but only the sign of the difference is
   used, so the two reads would end up as the operands of a MINUS inside
   the compare, (compare (minus (mem x) (mem y)) (const_int 0)), rather
   than directly as the compare's operands.  x must still be read first.  */

/*
**f:
**...
**	movl	x(|\(%rip\)), %eax
**	cmpl	y(|\(%rip\)), %eax
**...
*/

extern volatile int x, y;
void g1 (void);
void g2 (void);

void
f (void)
{
  int a = x;
  int b = y;
  if ((int) ((unsigned) a - (unsigned) b) < 0)
    g1 ();
  else
    g2 ();
}
