/* PR debug/100515 */
/* { dg-do compile } */
/* { dg-require-effective-target fopenmp } */
/* { dg-options "-g -O2 -fopenmp" } */

void
foo (int x)
{
#pragma omp taskloop
  for (int i = 0; i < x; i++)
    ;
}

void
bar (int x)
{
#pragma omp taskloop
  for (int i = 0; i < x; i++)
    ;
}
