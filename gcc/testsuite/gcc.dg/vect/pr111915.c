/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-vrp -fno-tree-dominator-opts -fno-tree-ccp" } */

void
foo (int * __restrict a, int * __restrict b, int * __restrict w)
{
  for (int i = 0; i < 16; ++i)
    {
      *a += w[2*i+0];
      *b += w[2*i&1];
    }
}
