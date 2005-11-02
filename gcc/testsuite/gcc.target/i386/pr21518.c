/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -fno-tree-pre" } */
/* { dg-require-effective-target ilp32 } */

extern void __attribute__ ((regparm (3)))
drawPointsLines (char type, int first, int *dd);

int
do_locator (int *call)
{
  char prephitmp5;
  int type;
  int i;

  if (call == 0)
    prephitmp5 = 1;
  else
    {
      type = *call;
      i = 0;
      do
	{
	  if (i != type)
	    drawPointsLines ((int) (char) type, 0, call);
	  i = i + 1;
	}
      while (i != 2);
      prephitmp5 = (char) type;
    }
  drawPointsLines ((int) prephitmp5, 0, call);
  return 0;
}
