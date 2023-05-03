/* { dg-options "-O2 -Wpsabi" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

enum __attribute__((aligned(16))) e { E };

/*
** test:
**	mov	w0, w1
**	ret
*/
enum e test (int x, enum e y) { return y; } /* { dg-bogus {parameter passing} } */
