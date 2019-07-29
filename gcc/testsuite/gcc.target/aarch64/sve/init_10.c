/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Case 5.4: Interleaved repeating elements and non-repeating elements.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

/*
** foo:
**	mov	(z[0-9]+\.s), w3
**	mov	(z[0-9]+\.s), w2
**	insr	\2, w1
**	insr	\2, w0
**	zip1	\2, \2, \1
**	...
*/
__attribute__((noipa))
vnx4si foo(int a, int b, int c, int f)
{
  return (vnx4si) { a, f, b, f, c, f, c, f };
}
