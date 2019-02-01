/* { dg-do compile } */
/* { dg-options "-O1 -floop-parallelize-all -fno-tree-loop-im --param scev-max-expr-size=1" } */

int au;

void
a8 (int k7)
{
  int xo;

  for (xo = 0; xo < 2; ++xo)
    {
      int dd;

      for (dd = 0; dd < 2; ++dd)
	au = !!k7 ? xo : 0;
    }
}
