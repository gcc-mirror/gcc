/* PR c/107001 */
/* { dg-do compile } */
/* { dg-options "-O0 -fopenmp -fexceptions" } */
/* { dg-require-effective-target exceptions } */

void bar (void);
void foo (void)
{
  #pragma omp taskgroup
  {
    #pragma omp taskgroup
    bar ();
  }
}
