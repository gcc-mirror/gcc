/* PR tree-optimization/90211 */
/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O3 -fassociative-math -ftree-parallelize-loops=2 -fno-signed-zeros -fno-trapping-math -fno-tree-copy-prop" } */

double
foo (int x)
{
  double a, b = 0.0;
  while (x < 3)
    {
      int c;
      a = 0.0;
      c = 0;
      while (c < x)
        {
          a += 1.0;
          ++c;
        }
      b += 1.0;
      ++x;
    }
  return a + b;
}
