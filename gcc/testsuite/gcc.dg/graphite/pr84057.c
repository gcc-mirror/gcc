/* { dg-do compile } */
/* { dg-options "-O2 -fgraphite -funroll-loops -fno-tree-ccp -fno-tree-dce" } */

int ue;

void
fr (int ct)
{
  int au = 0;
  int *ra = &au;

  while (au < 1)
    {
      au -= 0x7878788;
      if (au != ct && ue != 0)
	{
	  while (au < 1)
	    {
	    }

fc:
	  while (ct != 0)
	    {
	    }
	}
    }

  for (au = 0; au < 2; ++au)
    if (ct != 0)
      goto fc;
}
