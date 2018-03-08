/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize -fno-tree-copy-prop -fno-tree-fre -fno-tree-loop-ivcanon" } */

int cx;

int
e6 (int pj, int xe)
{
  for (cx = 0; cx < 2; ++cx)
    while (xe < 1)
      {
	for (cx = 0; cx < 2; ++cx)
	  pj *= 2;

	if (cx != 0)
	  goto o3;

	++xe;
      }

o3:
  return pj;
}
