/* { dg-options "-O -msve-vector-bits=256" } */

typedef unsigned char int8x4_t __attribute__((vector_size (4)));

/*
** passthru_x0:
**	ret
*/
int8x4_t passthru_x0 (int8x4_t x0) { return x0; }

/*
** passthru_x1:
**	mov	w0, w1
**	ret
*/
int8x4_t passthru_x1 (int8x4_t x0, int8x4_t x1) { return x1; }

int8x4_t load (int8x4_t *x0) { return *x0; }

void store (int8x4_t *x0, int8x4_t x1) { *x0 = x1; }

/*
** stack_callee:
**	ptrue	p[0-7], vl32
**	ld1b	(z[0-9]+\.d), \1/z, \[sp\]
**	st1b	\2, \1, \[x0\]
**	ret
*/
__attribute__((noipa))
void stack_callee (int8x4_t *x0, int8x4_t x1, int8x4_t x2, int8x4_t x3,
		   int8x4_t x4, int8x4_t x5, int8x4_t x6, int8x4_t x7,
		   int8x4_t stack0)
{
  *x0 = stack0;
}

/*
** stack_callee:
**	\.\.\.
**	ptrue	p[0-7], vl32
**	\.\.\.
**	ld1b	(z[0-9]+\.d), \1/z, \[x0\]
**	\.\.\.
**	st1b	\2, \1, \[sp\]
**	\.\.\.
**	ret
*/
void stack_caller (int8x4_t *x0, int8x4_t x1)
{
  stack_callee (x0, x1, x1, x1, x1, x1, x1, x1, *x0);
}

/* { dg-final { scan-assembler {\tmov\tw2, w} } } */
/* { dg-final { scan-assembler {\tmov\tw3, w} } } */
/* { dg-final { scan-assembler {\tmov\tw4, w} } } */
/* { dg-final { scan-assembler {\tmov\tw5, w} } } */
/* { dg-final { scan-assembler {\tmov\tw6, w} } } */
/* { dg-final { scan-assembler {\tmov\tw7, w} } } */
