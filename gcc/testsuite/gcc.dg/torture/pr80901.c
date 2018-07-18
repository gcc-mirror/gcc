/* { dg-do compile } */

int a, b, c, d;

int fn1 ()
{
  int f;
  if (d)
    while (c)
      {
	for (f = 0; f < 1; f++)
	  {
	    int g[70] = { 0 };
	    if (b)
	      ;
	    else
	      {
		int h = !b;
		for (; h; h = 1)
		  ;
	      }
	  }
	return 0;
      }
  return a;
}

int main ()
{
  fn1 ();
  return 0;
}
