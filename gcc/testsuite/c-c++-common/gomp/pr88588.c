/* PR middle-end/88588 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -O1" } */

int *v;

#pragma omp declare simd
void
foo (int x)
{
  int *a = &x;

  for (;;)
    {
      *v = *a;
      a = v;
    }
}
