/* PR c++/63249 */
/* { dg-do compile } */
/* { dg-options "-Wall -W -fopenmp" } */

int
foo (int *v, int A, int B)	/* { dg-bogus "set but not used" } */
{
  int r = 0;
  int a = 2;			/* { dg-bogus "set but not used" } */
  int b = 4;			/* { dg-bogus "set but not used" } */
#pragma omp target map(to: v[a:b])
  r |= v[3];
#pragma omp target map(to: v[A:B])
  r |= v[3];
  return r;
}
