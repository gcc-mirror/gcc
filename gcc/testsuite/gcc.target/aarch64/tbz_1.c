/* { dg-do compile } */
/* { dg-additional-options "-O2 -std=c99  -fno-unwind-tables -fno-asynchronous-unwind-tables" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#include <stdbool.h>

void h(void);

/*
** g1:
** 	tbnz	w[0-9]+, #?0, .L([0-9]+)
** 	ret
**	...
*/
void g1(bool x)
{
  if (__builtin_expect (x, 0))
    h ();
}

/*
** g2:
** 	tbz	w[0-9]+, #?0, .L([0-9]+)
** 	b	h
**	...
*/
void g2(bool x)
{
  if (__builtin_expect (x, 1))
    h ();
}

/*
** g3_ge:
** 	tbnz	w[0-9]+, #?31, .L[0-9]+
** 	b	h
**	...
*/
void g3_ge(int x)
{
  if (__builtin_expect (x >= 0, 1))
    h ();
}

/*
** g3_gt:
** 	cmp	w[0-9]+, 0
** 	ble	.L[0-9]+
** 	b	h
**	...
*/
void g3_gt(int x)
{
  if (__builtin_expect (x > 0, 1))
    h ();
}

/*
** g3_lt:
** 	tbz	w[0-9]+, #?31, .L[0-9]+
** 	b	h
**	...
*/
void g3_lt(int x)
{
  if (__builtin_expect (x < 0, 1))
    h ();
}

/*
** g3_le:
** 	cmp	w[0-9]+, 0
** 	bgt	.L[0-9]+
** 	b	h
**	...
*/
void g3_le(int x)
{
  if (__builtin_expect (x <= 0, 1))
    h ();
}

/*
** g5:
** 	mov	w[0-9]+, 65279
** 	tst	w[0-9]+, w[0-9]+
** 	beq	.L[0-9]+
** 	b	h
**	...
*/ 
void g5(int x)
{
  if (__builtin_expect (x & 0xfeff, 1))
    h ();
}
