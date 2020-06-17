/* PR tree-optimization/94166 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

typedef int __m128i __attribute__((__may_alias__, __vector_size__(4 * sizeof (int))));
unsigned int b[512];

void
foo (unsigned int *x, __m128i *y)
{
#define A(n) __m128i v##n = y[n];
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) A(n##5) A(n##6) A(n##7) \
	     A(n##8) A(n##9) A(n##a) A(n##b) A(n##c) A(n##d) A(n##e) A(n##f)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) B(n##5) B(n##6) B(n##7)
  C(0x)
#undef A
#define A(n) *(__m128i *) &b[4 * n] = v##n;
  C(0x)
#undef A
#define A(n) + b[4 * n] + b[4 * n + 1] + b[4 * n + 2] + b[4 * n + 3]
  *x = *x
  C(0x)
  ;
}
