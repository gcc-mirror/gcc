/* { dg-do compile } */

int a, b;
short c;

void f(void)
{
  int a = 0;
  int *k = 0;

  for(; a < 2; a++);

  if(!!(b |= a < 3) - 1)
    {
      if(0)
	for (;; a++)
	  {
	    for (; c; *k = 0);
lbl1:
	    ;
	  }

      for(; *k; k++)
	{
	  c = b ? : a;

	  if (c)
	    lbl2:
		b = 0;
	}
      goto lbl1;
    }

  for(;; b++)
    {
      if(b)
	goto lbl2;

      k = &b;
    }
}
