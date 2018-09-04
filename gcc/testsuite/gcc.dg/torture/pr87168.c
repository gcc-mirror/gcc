/* { dg-do compile } */

int a, b, c, d, e, f, *g;

int main ()
{ 
  unsigned i;
  while (b)
    { 
      int j, m;
L1:
      f = j;
L2:
      if (i && e)
	{ 
	  i = f;
	  goto L2;
	}
      j = f;
      if (a)
	goto L3;
      for (m = 0; m < 2; m++)
	if (d)
	  goto L1;
      goto L2;
L3:
      (&j != g) | c;
    }
  return 0;
}
