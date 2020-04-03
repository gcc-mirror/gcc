/* PR c++/94385 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int V __attribute__((__vector_size__(16)));
typedef float W __attribute__((__vector_size__(16)));

void
foo (W *x, V *y)
{
  *y = (({ __builtin_convertvector (*x, V); }));
}
