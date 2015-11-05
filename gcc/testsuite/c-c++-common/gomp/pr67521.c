/* PR middle-end/67521 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (int x)
{
  int i = 0;
  #pragma omp parallel for simd
  for (i = (i & x); i < 10; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  i = 0;
  #pragma omp parallel for simd
  for (i = 0; i < (i & x) + 10; i = i + 2) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  i = 0;
  #pragma omp parallel for simd
  for (i = 0; i < 10; i = i + ((i & x) + 2)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
}
