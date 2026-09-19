/* The equality a conditional branch tests holds on one of its edges; reload
   CSE must see it through the condition-code register, so that re-creating
   the same value in the target block is dropped and the exit can become a
   conditional return.  */
/* { dg-do compile } */
/* { dg-options "-O2 -marm" } */
/* { dg-require-effective-target arm_arch_v6_arm_ok } */
/* { dg-add-options arm_arch_v6 } */
/* { dg-add-options check_function_bodies } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int g (int);
int h (int);

/*
** zero:
**	subs	r3, r0, #0
**	bxeq	lr
**	push	{r4, lr}
**	bl	g
**	cmp	r0, #0
**	popeq	{r4, pc}
**	pop	{r4, lr}
**	b	h
*/
int
zero (int x)
{
  if (x == 0)
    return 0;
  x = g (x);
  if (x == 0)
    return 0;
  return h (x);
}

/*
** nonzero:
**	cmp	r0, #7
**	bxeq	lr
**	push	{r4, lr}
**	bl	g
**	cmp	r0, #7
**	popeq	{r4, pc}
**	pop	{r4, lr}
**	b	h
*/
int
nonzero (int x)
{
  if (x == 7)
    return 7;
  x = g (x);
  if (x == 7)
    return 7;
  return h (x);
}
