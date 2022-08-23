/* PR tree-optimization/105458 */
/* { dg-do compile } */
/* { dg-options "-O1 -fexpensive-optimizations -fno-tree-dominator-opts " } */

void
yj (int j4)
{
  int t3;

  for (t3 = 0; t3 < 6; ++t3)
    {
      short int v4 = t3;

      if (v4 == j4 || v4 > t3)
        for (;;)
          {
          }
    }
}

