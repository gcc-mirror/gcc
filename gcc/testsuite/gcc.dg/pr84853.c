/* PR c/84853 */
/* { dg-do compile } */

typedef float V __attribute__((__vector_size__ (16)));
typedef int W __attribute__((__vector_size__ (16)));

void
foo (int x, V *y, V *z, W *w)
{
  *y = *y << x;		/* { dg-error "invalid operands to binary <<" } */
  *z = *z << *w;	/* { dg-error "invalid operands to binary <<" } */
}

void
bar (int x, V *y, V *z, W *w)
{
  *y = *y >> x;		/* { dg-error "invalid operands to binary >>" } */
  *z = *z >> *w;	/* { dg-error "invalid operands to binary >>" } */
}
