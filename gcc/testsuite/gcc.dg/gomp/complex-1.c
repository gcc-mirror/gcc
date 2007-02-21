/* { dg-do compile } */
/* { dg-options "-fopenmp -O1" } */
/* PR middle-end/30143 */


int f (int n)
{
  int i;
  _Complex float t;
#pragma omp parallel
    for (i = 1; i < n - 1; ++i)
      t+=1;
}
