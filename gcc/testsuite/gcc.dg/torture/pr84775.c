/* { dg-do compile } */

int a, b, c, d, e, f;

void fn1 ()
{
  while (e)
    for (f = 0; f < 4; f++)
      {
	int h[1];
	for (; c; c++)
	  if (a)
	    break;
	d = a - 4;
	if (d)
	  b = h[0];
      }
}
