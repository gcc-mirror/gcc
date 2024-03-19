/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=pac-ret+leaf -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
**foo:
**	hint	25 // paciasp
**	...
**	cbz	w2, .*
**	mov	x4, 0
**	...
**	cbz	x4, .*
**	add	sp, sp, x5
**	br	x6
** (
**	hint	29 // autiasp
**	ret
** |
**	retaa
** )
**	mov	x5, x0
**	mov	x4, 1
**	mov	x6, x1
**	b	.*
*/
void
foo (unsigned long off, void *handler, int c)
{
  if (c)
    return;
  __builtin_eh_return (off, handler);
}
