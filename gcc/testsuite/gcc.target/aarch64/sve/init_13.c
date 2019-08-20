/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef float vnx4sf __attribute__((vector_size (32)));

/*
** foo:
**	mov	(z[0-9]+\.s), s0
**	insr	\1, wzr
**	...
*/
vnx4sf
foo (float a)
{
  return (vnx4sf) { 0.0f, a, a, a, a, a, a, a };
}
