/* { dg-do compile } */

int a, b, c, d, e;
char f, g;

void fn1 ()
{
  while (1)
    {
      if (d)
	goto L1;
      if (e)
	goto L3;
      int q = (c && a) % (f * (d || a)) && b;
      e = q;
      if (b)
	break;
L1:
L2:
      c = f;
L3:
      f = g;
      while (a)
	goto L2;
    }
}
