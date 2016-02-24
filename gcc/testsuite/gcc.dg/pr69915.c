/* PR middle-end/69915 */
/* { dg-do compile } */
/* { dg-options "-O -ftracer" } */

typedef unsigned short V __attribute__ ((vector_size (32)));

unsigned
foo (unsigned x, unsigned c, V *p)
{
  V v = *p;
  if (c < 360)
    v = (V) { 0 };
  v *= (V) { x };
  return v[1];
}
