/* PR middle-end/59669 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare simd uniform(a) aligned(a:32)
void
bar (int *a)
{
}
