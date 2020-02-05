/* PR middle-end/93555 */
/* { dg-do compile } */

#pragma omp declare simd
#pragma omp declare simd inbranch
void
foo (void)
{
}

#pragma omp declare simd inbranch
#pragma omp declare simd
void
bar (void)
{
}
