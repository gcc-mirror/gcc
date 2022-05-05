/* PR middle-end/104522 */
/* { dg-do compile } */
/* { dg-options "-O -fcompare-debug -dP" } */

typedef short __attribute__((__vector_size__(16))) V;
long double x;

void
foo (void)
{
  V t = { 512, 0, 0, 0, 16384 };
  long double u = *(long double *) &t;
  x /= u;
}
