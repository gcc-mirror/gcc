/* PR middle-end/98183 */
/* { dg-additional-options "-fexceptions -O0" } */

void bar (void);
int x, y;

void
foo (void)
{
#pragma omp target data map(tofrom: x)
  {
#pragma omp target data map(tofrom: y)
    bar ();
  }
}
