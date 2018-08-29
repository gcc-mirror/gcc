/* { dg-do compile } */
/* { dg-options "-O2 -fgraphite-identity -fno-tree-copy-prop --param lim-expensive=3" } */

unsigned int dj;

void
np (void)
{
  const unsigned int uw = 2;
  unsigned int eu;

  for (eu = 0; eu < uw; ++eu)
    {
      for (dj = 0; dj < uw; ++dj)
	;
      eu -= !!(dj - uw - 1);
    }
}
