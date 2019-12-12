/* PR c/29955 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fexceptions" } */
/* { dg-require-effective-target exceptions } */

extern void bar (int);

void
foo (int n)
{
  int i;
#pragma omp parallel for schedule(dynamic)
  for (i = 0; i < n; i++)
    bar (0);
}
