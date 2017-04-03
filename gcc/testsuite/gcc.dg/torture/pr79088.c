/* { dg-do run } */

int a, b, c, d, e = 1;

int main ()
{
  int f;
  if (a)
    goto L;
  for (f = 0; f < e; e++)
    {
L:
      if (d)
	continue;
      if (c)
	goto L;
      for (a = 0; a < 6; a++)
	for (f = 0; f < 3; f++)
	  while (b)
	    c++;
    }
  return 0;
}
