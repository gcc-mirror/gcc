/* { dg-do compile } */
/* { dg-options "-O2  -fno-tree-dce -fno-tree-dse" } */
/* PR tree-optimization/126569 */

int wx, qi, gw, ak;

void
f (void)
{
  while (wx)
    {
      int k2 = ak;
    ja:
      *(int **) &k2 = &gw;
      while (qi)
        ++qi;
    }
  gw = 1;
  goto ja;
}
