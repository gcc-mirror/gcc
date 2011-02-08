/* { dg-options "-O -fgraphite-identity -ffast-math -fno-tree-dce" } */

void foo ()
{
  int M0[4][4], M3[4] = {};
  int i=-1;
  int ii, jj;
  for (; i; i++)
      for (jj = 0; jj < 4; jj++)
	for (ii = 0; ii < 4; ii++)
	    M3[1] += __builtin_abs (M0[ii][0]);
}
