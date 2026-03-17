/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target dfp } */

/* PR rtl-optimization/123822 */

typedef __attribute__((__vector_size__(16))) _Decimal64 D;

int i;
D d;

void
foo()
{
  d /= (D)(d >= *(D *)__builtin_memset(&d, i, 3));
}
