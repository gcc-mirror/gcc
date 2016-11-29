/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Testing some loops.  */

int f (void);

int
g (int i)
{
  switch (i)
    {
    case 0:
      for (;;)
	{
	  if (f ()) /* { dg-warning "statement may fall through" "fall through" { xfail *-*-* } } */
	    break;
	}
    case 1:
      return 1;
    }
  return 0;
}

int
h (int i)
{
  switch (i)
    {
    case 0:
      do
	{
	  if (f ()) /* { dg-warning "statement may fall through" } */
	    break;
	}
      while (0);
    case 1:
      return 1;
    }
  return 0;
}
