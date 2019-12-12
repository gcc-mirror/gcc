/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dominator-opts" } */

int nv;

void test (void)
{
  #pragma GCC unroll 2
  for (nv = 0; nv < 1; ++nv)
    {}
}
