/* { dg-do compile } */
/* { dg-options "-O -fno-tree-loop-optimize" } */

void test (void)
{
  #pragma GCC unroll 2
  for (int nv = 0; nv <= 2; nv += 2)
    {}
}
