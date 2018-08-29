/* PR c++/84341 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (int i)
{
  #pragma omp atomic
    i = &i + 1;		/* { dg-error "invalid form of" } */
}
