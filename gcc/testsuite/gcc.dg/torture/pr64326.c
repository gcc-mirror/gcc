/* { dg-do compile } */

int a, b, c, d, e, f[5][2];
char g;

int
fn1 ()
{
  return d && c ? 0 : 1;
}

int
fn2 ()
{
  int h;
  for (;;)
    for (; e;)
      {
	int i, j;
	h = a ? 1 : b;
	if (h || fn1 () ^ g - 682)
	  {
	    for (i = 0; i < 5; i++)
	      for (j = 0; j < 2; j++)
		f[i][j] = 0;
	    return 0;
	  }
      }
}
