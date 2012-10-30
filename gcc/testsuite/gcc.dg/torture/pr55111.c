/* { dg-do compile } */

int a, b, c;
long d;
unsigned long *e;

int f(void)
{
  for(;; a++)
    {
      if(c)
	{
	  for(b = d = 0; b < 1; b++)
	    e = &d;

	  --*e;

	  if(d > 0)
	    a = 0;

	  return d;
	}
    }
}
