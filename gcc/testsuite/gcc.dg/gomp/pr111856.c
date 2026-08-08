/* PR middle-end/111856 */
/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -O2" } */

typedef void T;
int array[1000];
#pragma omp declare simd notinbranch simdlen(4)
T foo (int i)
{
  array[i] = 555;
}
