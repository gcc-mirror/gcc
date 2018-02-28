/* { dg-do compile } */
/* { dg-options "-O -fgraphite-identity -fno-tree-loop-im" } */

int q3, w1;

void
bw (int b8)
{
  const int sd = 2;
  int mc;

  for (mc = 0; mc < sd; ++mc)
    {
ik:
      for (w1 = 0; w1 < sd; ++w1)
	++b8;
    }

  for (q3 = 0; q3 < sd; ++q3)
    ;

  goto ik;
}
