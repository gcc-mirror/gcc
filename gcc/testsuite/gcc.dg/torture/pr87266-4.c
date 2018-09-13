/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dominator-opts -fno-tree-forwprop" } */

unsigned long int
re (long int j9)
{
  if (j9 == 0)
    return 1;

  return j9;
}

void
zq (int bt, int yy)
{
  int p3 = 0, go = 4, ez = go;

  while (go != 0)
    {
      if (ez + !!bt - re (bt) != 0 && go != 0)
	{
	  if (yy != 0)
	    p3 = yy;
	}
      else
	return;

      go = 2;
    }
}

void
my (unsigned long int n6, int bt, int yy)
{
  zq (bt, yy);
  n6 = n6 == bt;
  zq (bt, yy);
}
