/* PR tree-optimization/87895 */
/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#pragma omp declare simd
int foo (int x) __attribute__((noreturn));

#pragma omp declare simd
int
bar (int x, int y)
{
  if (y == 1)
    foo (x + 2);
  if (y == 10)
    foo (x + 6);
  if (y != 25)
    return 4;
}
