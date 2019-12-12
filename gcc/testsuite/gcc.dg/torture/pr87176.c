/* { dg-do run } */

int a, b, c;

int main ()
{
  int d = a = 0;
  while (1)
    {
      a = a ^ 6;
      if (!a)
	break;
      if (d)
	goto L;
      d = a;
      for (b = 0; b < 2; b++)
	{
	  const int *f[3] = { &c };
	  const int **g[] = { &f[2] };
	  int h = ~d;
	  if (d)
	    L:
		if (h > 1)
		  continue;
	}
    }
  return 0;
}
