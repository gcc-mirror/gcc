/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi" } */

typedef float __attribute__((__vector_size__ (2 * sizeof(float)))) F;
typedef double __attribute__((__vector_size__ (2 * sizeof(double)))) G;

F f;
G g;

F
foo (void)
{
  G h = __builtin_convertvector (f, G);
  g = h <= h;
  return f;
}
