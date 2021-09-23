/* PR rtl-optimization/101008 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

typedef unsigned __attribute__((__vector_size__(32))) U;
typedef unsigned __attribute__((__vector_size__(16))) V;

int c, r;

V v;

void
foo(void)
{
  U u = __builtin_shufflevector (v, (V)(v != c) | (V)(c == v),
				 4, 3, 5, 5, 1, 2, 3, 0);
  r = ((union { U a; int b; }) u).b;
}
