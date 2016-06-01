/* { dg-do compile } */

char a[1];
int b;
void fn1()
{
  int i;
  for (;;)
    {
      i = 0;
      for (; i < 6; i++)
	{
	  if (b)
	    for (;;)
	      ;
	  int c = a[i];
	  __builtin_printf("", i);
	}
    }
}
