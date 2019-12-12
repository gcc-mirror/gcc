/* PR tree-optimization/90866 - ICE in fold_binary_loc, at fold-const.c:9827
   { dg-do compile  }
   { dg-options "-O3 -Wall -fno-tree-loop-optimize" } */

int a[1024], b[1024];

void f (void)
{
  int i = 0;
  for ( ; ; i++)
    {
    b[16] = a[i * 16 + 10];
    b[i * 16 + 11] = a[i * 16 + 11] * 3;
    b[i * 16 + 12] = a[i * 16 + 12] * 4;
    b[i * 16 + 13] = a[i * 16 + 13] * 4;
    b[i * 16 + 14] = a[i * 16 + 14] * 3;
  }
}
