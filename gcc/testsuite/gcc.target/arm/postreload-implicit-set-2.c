/* As postreload-implicit-set-1.c, in Thumb-2 state.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mthumb" } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-add-options arm_arch_v7a } */
/* { dg-add-options check_function_bodies } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int g (int);
int h (int);

/*
** nonzero:
**	cmp	r0, #7
**	beq	\.L[0-9]+
**	push	{r3, lr}
**	bl	g
**	cmp	r0, #7
**	beq	\.L[0-9]+
**	pop	{r3, lr}
**	b	h
**	pop	{r3, pc}
**	bx	lr
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
