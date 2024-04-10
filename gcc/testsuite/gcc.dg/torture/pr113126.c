/* { dg-do compile } */

typedef float __attribute__((__vector_size__ (8))) F;
typedef double __attribute__((__vector_size__ (16))) G;

F f;
G g;

F
foo (void)
{
  G h = __builtin_convertvector (f, G);
  g = h <= h;
  return f;
}
