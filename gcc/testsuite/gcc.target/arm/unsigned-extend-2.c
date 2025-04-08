/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2 -mthumb" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
** 	movs	(r[0-9]+), #8
** (
** 	subs	\1, \1, #1
** 	ands	\1, \1, #255
** 	and	r0, r1, r0, lsr #1
** 	bne	.L[0-9]+
** 	bx	lr
** |
** 	subs	\1, \1, #1
** 	and	r0, r1, r0, lsr #1
** 	ands	\1, \1, #255
** 	bne	.L[0-9]+
** 	bx	lr
** |
** 	push	{lr}
** 	dls	lr, \1
** 	and	r0, r1, r0, lsr #1
** 	le	lr, .L[0-9]+
** 	pop	{pc}
** )
*/

unsigned short foo (unsigned short x, unsigned short c)
{
  unsigned char i = 0;
  for (i = 0; i < 8; i++)
    {
      x >>= 1;
      x &= c;
    }
  return x;
}
