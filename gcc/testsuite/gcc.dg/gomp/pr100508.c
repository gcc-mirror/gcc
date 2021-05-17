/* PR middle-end/100508 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fopenmp-simd" } */

typedef int __attribute__((__vector_size__(32))) V;
V j;

#pragma omp declare simd
int
foo (void)
{
  V m = j;
  return 0;
}
