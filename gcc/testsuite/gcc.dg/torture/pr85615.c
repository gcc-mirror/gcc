/* { dg-do compile } */

long a, d;
int b, c;
void fn1()
{
  int e = -1L, f = 2, g = 8;
  for (;;)
    {
      for (; b; g++)
	;
      int i;
      for (; c;)
	{
	  i = 5;
	  for (; e >= 1; i--)
	    ;
	}
      d = f ?: a;
      if (d)
	{
	  e = 0;
	  for (; i;)
	    for (; g < 3; f++)
	      ;
	}
    }
}
