/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.6-a+mops --param=aarch64-mops-memset-size-threshold=0" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdlib.h>

/* We want to inline variable-sized memset.
** do_it_set:
**	setp	\[x0\]\!, x2\!, x1
**	setm	\[x0\]\!, x2\!, x1
**	sete	\[x0\]\!, x2\!, x1
**	ret
*/
void do_it_set (char * out, int n, size_t size)
{
  __builtin_memset (out, n, size);
}

/*
** do_it_set_large:
**	mov	w2, 1
**	mov	x1, 1024
**	setp	\[x0\]\!, x1\!, x2
**	setm	\[x0\]\!, x1\!, x2
**	sete	\[x0\]\!, x1\!, x2
**	ret
*/
void do_it_set_large (char * out)
{
  __builtin_memset (out, 1, 1024);
}

/*
** do_it_set_256:
**	mov	w2, 1
**	mov	x1, 256
**	setp	\[x0\]\!, x1\!, x2
**	setm	\[x0\]\!, x1\!, x2
**	sete	\[x0\]\!, x1\!, x2
**	ret
*/
void do_it_set_256 (char * out)
{
  __builtin_memset (out, 1, 256);
}

/*
** do_it_set_255:
**	mov	w2, 1
**	mov	x1, 255
**	setp	\[x0\]\!, x1\!, x2
**	setm	\[x0\]\!, x1\!, x2
**	sete	\[x0\]\!, x1\!, x2
**	ret
*/
void do_it_set_255 (char * out)
{
  __builtin_memset (out, 1, 255);
}

/*
** do_it_set_0:
**	setp	\[x0\]\!, x1\!, xzr
**	setm	\[x0\]\!, x1\!, xzr
**	sete	\[x0\]\!, x1\!, xzr
**	ret
*/
void do_it_set_0 (char * out, size_t n)
{
  __builtin_memset (out, 0, n);
}

/*
** do_it_set_0_255:
**	mov	x1, 255
**	setp	\[x0\]\!, x1\!, xzr
**	setm	\[x0\]\!, x1\!, xzr
**	sete	\[x0\]\!, x1\!, xzr
**	ret
*/
void do_it_set_0_255 (char * out)
{
  __builtin_memset (out, 0, 255);
}

