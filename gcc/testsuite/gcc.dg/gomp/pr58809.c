/* PR middle-end/58809 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -O" } */

int i;
#pragma omp threadprivate (i)

void foo()
{
  _Complex int j;
#pragma omp parallel copyin (i) reduction (&&:j)
  ;
}
