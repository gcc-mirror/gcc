/* { dg-do compile } */
/* { dg-options "-O -fssa -fssa-ccp" } */

double a[10][35], b[10][8];
int c, c, d, e, f, g, h;

int foo ()
{
  int i, j, k, l;

  if (c > 10)
    c = 10;

  for (j = 0; j < c; j++)
    {
      k = 0;
      for (l = 0; l < h; l++)
	{
	  if (d != 5)
	    return -1;
	  k = l * g;
	  a[j][k] = (double) e; k++;
	  a[j][k] = (double) f; k++;
	}
      for (i = 0;i < 35; i++)
        {
	  if (a[j][i] >= 0.9)
	    a[j][i] = 0.9;
	  if (a[j][i] <= 0.1)
	    a[j][i] = 0.1;
        }
      k = 0;
      b[j][k] = (double) e; k++;
      b[j][k] = (double) f; k++;
    }
  return 0;
}
