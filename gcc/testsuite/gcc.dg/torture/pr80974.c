/* { dg-do run } */

int a, b, c, d, e, f, g[4];

static int fn1 ()
{
  int h, i;
  if (b)
    goto L1;
L2:;
   int m = a;
   while (1)
     {
       int n = 2;
       e = !f && (n = 5);
       if (e)
	 for (h = 0; h < 9; h++)
	   for (i = 0; i < 6; i++)
	     if (a)
	       g[h] = 4;
       if (d)
	 goto L2;
       a & n || b;
L1:
       if (a)
	 L3:
	     c = m;
       if (a)
	 goto L3;
       if (b < 5)
	 return 0;
     }
}

int main ()
{
  fn1 ();
  return 0; 
}
