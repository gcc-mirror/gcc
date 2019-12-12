/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Case 1.1: Trailing constants with stepped sequence.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

/*
** foo:
**	index	(z[0-9]+\.s), #1, #1
**	insr	\1, w1
**	insr	\1, w0
**	...
*/
__attribute__((noipa))
vnx4si foo(int a, int b)
{
  return (vnx4si) { a, b, 1, 2, 3, 4, 5, 6 };
}
