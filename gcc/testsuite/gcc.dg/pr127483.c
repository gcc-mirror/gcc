/* PR rtl-optimization/127483 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -g" } */

typedef __attribute__((__vector_size__(16))) __int128 W;

long c;
W g;

void
foo()
{
  W w = g << c;
  (struct {W m;}){w};
}
