/* { dg-do run } */
/* { dg-options "-O -fno-tree-dominator-opts -fgraphite-identity" } */

int a, b, c;
int d[14];

int
main (void)
{
  short e;
  char f;

  for (; b >= 0; b--)
    {
      e = 0;
      for (; e < 2; e++)
	{
	  a = 0;
	  for (; a < 7; a++)
	    d[a] = 1;
	}
      if (c)
	{
	  f = 0;
	  for (; f >= 0; f--)
	    ;
	}
    }

  if (a != 7)
    __builtin_abort ();

  return 0;
}
