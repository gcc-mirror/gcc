/* PR tree-optimization/65735 */

int foo (void);

void
bar (int a, int b, int c)
{
  while (!a)
    {
      c = foo ();
      if (c == 7)
	c = b;
      switch (c)
	{
	case 1:
	  a = b++;
	  if (b)
	    b = 1;
	}
    }
}
