/* { dg-do compile } */

int a, b;

void
foo (int x)
{
  int e[2];
  int d;
  while (x)
    {
      for (d = 0; d <= 1; d = 1)
	if (e[a])
	  break;
      for (b = 0; b <= 0; b = 1)
	{
	  e[a] = a;
	  if (a)
	    break;
	}
    }
}
