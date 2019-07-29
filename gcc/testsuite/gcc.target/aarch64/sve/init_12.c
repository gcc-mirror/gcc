/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Case 5.5: Interleaved repeating elements and trailing same elements.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

/*
** foo:
**	mov	(z[0-9]+\.s), w2
**	mov	(z[0-9]+\.s), w0
**	insr	\2, w1
**	insr	\2, w1
**	insr	\2, w1
**	zip1	\2, \2, \1
**	...
*/
__attribute__((noipa))
vnx4si foo(int a, int b, int f) 
{
  return (vnx4si) { b, f, b, f, b, f, a, f };
}
