/* { dg-do compile } */

unsigned a, b, c;

void f(void)
{
  for(; a; a++)
    {
      long *p1 = (long *)&b;

      if(*p1)
	return;

      if(b && (*p1 = b) || c && ++*p1)
	{
	  unsigned *p2 = &b;

	  for(*p2 = 0; *p2 < 1;)
	    for(; b; b++);
	}
    }
}
