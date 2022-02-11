/* PR middle-end/71691 */
/* { dg-do run } */
/* { dg-options "-fno-tree-vrp -O2 -funswitch-loops" } */

/* Note: The -fno-tree-vrp above is only there to avoid VRP papering
   over the problem.  */

char b;
short f;
unsigned e;
int g = 20;

void
foo ()
{
  int l, h;
  for (l = 0; l <= 7; l++)
    {
      int j = 38;
      if (g)
	h = 0;
      for (; h <= 7; h++)
	{
	  int i, k = b % (j % 4);
	  g = f;
	  for (;;)
	    {
	      j = 6 || b;
	      if (e)
		{
		  for (; j; --j)
		    if (k)
		      __builtin_printf ("%d", 9);
		  if (i)
		    __builtin_printf ("%d", j);
		}
	      if (l)
		continue;
	      break;
	    }
	  i = f || b;
	}
    }
}

int
main ()
{
  foo ();
  return 0;
}
