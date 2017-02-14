/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Another nested switch, and with an initialization on top.  Check that
   we do warn here.  */

void
f (int i)
{
  switch (i)
    {
    case 1:
      {
	int t = 3;
	switch (i)
	  {
	  case 3:
	    if (i > 5)
	      --i;
	    i += 10; /* { dg-warning "statement may fall through" } */
	  case 4:
	    t /= 5;
	    break;
	  }
	break;
      }
    case 2:
      --i;
      break;
    }
}
