/* PR tree-optimization/64769 */
/* { dg-do compile } */
/* { dg-options "-fopenmp-simd" } */

#pragma omp declare simd linear(i)
void
foo (int i)
{
}
