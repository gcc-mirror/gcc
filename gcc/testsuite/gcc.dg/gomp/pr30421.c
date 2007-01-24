/* PR middle-end/30421 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -Wall" } */

int
foo ()
{
  int a = 0, i;

#pragma omp parallel for firstprivate(a) lastprivate(a)
  for (i = 0; i < 10; i++)
    a += i;

  return a;
}

int
bar ()
{
  int a = 0, i;

#pragma omp parallel for firstprivate(a) lastprivate(a) schedule(static, 2)
  for (i = 0; i < 10; i++)
    a += i;

  return a;
}

int
baz ()
{
  int a = 0, i;

#pragma omp parallel for firstprivate(a) lastprivate(a) schedule(dynamic)
  for (i = 0; i < 10; i++)
    a += i;

  return a;
}
