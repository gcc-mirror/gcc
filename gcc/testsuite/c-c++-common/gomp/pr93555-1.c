/* PR middle-end/93555 */
/* { dg-do compile } */

#pragma omp declare simd
#pragma omp declare simd inbranch
int
foo (int x)
{
  return x;
}

#pragma omp declare simd inbranch
#pragma omp declare simd
int
bar (int x)
{
  return x;
}
