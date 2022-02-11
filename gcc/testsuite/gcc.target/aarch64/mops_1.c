/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.6-a+mops --param=aarch64-mops-memcpy-size-threshold=0" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdlib.h>

/* We want to inline variable-sized memcpy.
** do_it_cpy:
**	cpyfp	\[x1\]\!, \[x0\]\!, x2\!
**	cpyfm	\[x1\]\!, \[x0\]\!, x2\!
**	cpyfe	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_cpy (char * in, char * out, size_t size)
{
  __builtin_memcpy (out, in, size);
}

/*
** do_it_cpy_large:
**	mov	x2, 1024
**	cpyfp	\[x1\]\!, \[x0\]!, x2\!
**	cpyfm	\[x1\]\!, \[x0\]!, x2\!
**	cpyfe	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_cpy_large (char * in, char * out)
{
  __builtin_memcpy (out, in, 1024);
}

/*
** do_it_cpy_127:
**	mov	x2, 127
**	cpyfp	\[x1\]\!, \[x0\]!, x2\!
**	cpyfm	\[x1\]\!, \[x0\]!, x2\!
**	cpyfe	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_cpy_127 (char * in, char * out)
{
  __builtin_memcpy (out, in, 127);
}

/*
** do_it_cpy_128:
**	mov	x2, 128
**	cpyfp	\[x1\]\!, \[x0\]!, x2\!
**	cpyfm	\[x1\]\!, \[x0\]!, x2\!
**	cpyfe	\[x1\]\!, \[x0\]\!, x2\!
**	ret
*/
void do_it_cpy_128 (char * in, char * out)
{
  __builtin_memcpy (out, in, 128);
}

