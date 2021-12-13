/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.6-a+mops --param=aarch64-mops-memmove-size-threshold=0" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdlib.h>

/* We want to inline variable-sized memmove.
** do_it_mov:
**	cpyp	\[x1\]\!, \[x0\]\!, x2\!
**	cpym	\[x1\]\!, \[x0\]\!, x2\!
**	cpye	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_mov (char * in, char * out, size_t size)
{
  __builtin_memmove (out, in, size);
}

/*
** do_it_mov_large:
**	mov	x2, 1024
**	cpyp	\[x1\]\!, \[x0\]!, x2\!
**	cpym	\[x1\]\!, \[x0\]!, x2\!
**	cpye	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_mov_large (char * in, char * out)
{
  __builtin_memmove (out, in, 1024);
}

/*
** do_it_mov_127:
**	mov	x2, 127
**	cpyp	\[x1\]\!, \[x0\]!, x2\!
**	cpym	\[x1\]\!, \[x0\]!, x2\!
**	cpye	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_mov_127 (char * in, char * out)
{
  __builtin_memmove (out, in, 127);
}

/*
** do_it_mov_128:
**	mov	x2, 128
**	cpyp	\[x1\]\!, \[x0\]!, x2\!
**	cpym	\[x1\]\!, \[x0\]!, x2\!
**	cpye	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_mov_128 (char * in, char * out)
{
  __builtin_memmove (out, in, 128);
}

