/* { dg-do compile } */

char a;

void f(void)
{
  char b = 2;

  for(;;)
    {
      unsigned short s = 1, *p = &s, *i;

      for(*i = 0; *i < 4; ++*i)
	if(a | (*p /= (b += !!a)) <= 63739)
	  return;

      if(!s)
	a = 0;

      for(;;);
    }
}
