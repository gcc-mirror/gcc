/* { dg-options "-O2 -mcmodel=small" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** fun:
**	(ldr|adrp)	[^\n]+
**	(ldr|adrp)	[^\n]+
**	(ldr|adrp)	[^\n]+
**	(ldr|adrp)	[^\n]+
**	tbl	v[0-9]+.16b, {v[0-9]+.16b - v[0-9]+.16b}, v[0-9]+.16b
**	str	[^\n]+
**	ret
*/
typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));
void fun (veci *a, veci *b, veci *c) {
  *c = __builtin_shufflevector (*a, *b, 0, 5, 2, 7);
}

/* { dg-final { scan-assembler-not {\teor\t} } } */
