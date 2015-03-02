/* PR 65048 */
/* { dg-do compile } */
/* { dg-options "-O3 -std=gnu89" } */

int a, b, c, d;
void fn (void);

int
foo (x)
     int x;
{
  switch (x)
    {
    case 'A':
      return 'T';
    case 'U':
      return 'A';
    }
}

void
bar (int x, int y)
{
  switch (c)
    {
    case 'U':
      switch (x)
	{
	default:
	  fn ();
	case 'G':
	  switch (y)
	    {
	    case 'A':
	      d = 7;
	    }
	}
    }
}

void
baz (void)
{
  while (1)
    {
      a = foo ();
      b = foo ();
      bar (a, b);
    }
}

