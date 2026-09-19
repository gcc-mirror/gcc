/* As gcc.target/i386/postreload-implicit-set-1.c.  The zero case is already
   handled without this, because cbz carries the comparison in the branch;
   this covers the cmp form.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-add-options check_function_bodies } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int g (int);
int h (int);

/*
** seven:
**	cmp	w0, 7
**	beq	\.L[0-9]+
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	bl	g
**	cmp	w0, 7
**	beq	\.L[0-9]+
**	ldp	x29, x30, \[sp\], 16
**	b	h
**	ldp	x29, x30, \[sp\], 16
**	ret
**	ret
*/
int
seven (int x)
{
  if (x == 7)
    return 7;
  x = g (x);
  if (x == 7)
    return 7;
  return h (x);
}
