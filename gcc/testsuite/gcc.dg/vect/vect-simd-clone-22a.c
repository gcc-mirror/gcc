/* { dg-do compile } */

#pragma omp declare simd simdlen(16) inbranch
float baz (float x, float y)
{
  return x / y;
}
