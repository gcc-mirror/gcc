/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

/*
** zero_q:
**	movi	v0.2d, #0
**	ret
*/
void
zero_q ()
{
  register __int128_t q0 asm ("q0");
  q0 = 0;
  asm volatile ("" :: "w" (q0));
}

/*
** load_q:
**	ldr	q0, \[x0\]
**	ret
*/
void
load_q (__int128_t *ptr)
{
  register __int128_t q0 asm ("q0");
  q0 = *ptr;
  asm volatile ("" :: "w" (q0));
}

/*
** store_q:
**	str	q0, \[x0\]
**	ret
*/
void
store_q (__int128_t *ptr)
{
  register __int128_t q0 asm ("q0");
  asm volatile ("" : "=w" (q0));
  *ptr = q0;
}

/*
** zero_x:
** (
**	mov	x0, #?0
**	mov	x1, #?0
** |
**	mov	x1, #?0
**	mov	x0, #?0
** )
**	ret
*/
void
zero_x ()
{
  register __int128_t x0 asm ("x0");
  x0 = 0;
  asm volatile ("" :: "r" (x0));
}

/*
** load_x:
**	ldp	x2, x3, \[x0\]
**	ret
*/
void
load_x (__int128_t *ptr)
{
  register __int128_t x2 asm ("x2");
  x2 = *ptr;
  asm volatile ("" :: "r" (x2));
}

/*
** store_x:
**	stp	x2, x3, \[x0\]
**	ret
*/
void
store_x (__int128_t *ptr)
{
  register __int128_t x2 asm ("x2");
  asm volatile ("" : "=r" (x2));
  *ptr = x2;
}
