/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

/* PR middle-end/97898 */

void f (int n, int *s, int *e)
{
  int *i;
#pragma omp for schedule(static, 2)
  for (i = s; i < e; i += n);
}

