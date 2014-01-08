/* PR middle-end/59669 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare simd linear(a)
void
foo (int a)
{
}
