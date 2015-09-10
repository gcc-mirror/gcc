/* PR middle-end/67517 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int
foo (int x, int y, int z)
{
  int i;
  #pragma omp parallel for simd linear (y : x & 15) linear (x : 16) linear (z : x & 15)
  for (i = 0; i < 256; ++i)
    x += 16, y += x & 15, z += x & 15;
  return x + y + z;
}
