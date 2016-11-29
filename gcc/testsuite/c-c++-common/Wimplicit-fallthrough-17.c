/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Another nested switch, and with an initialization on top.  Check that
   we do not warn here as the case 3 falls through to break.  */

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
	    i += 10;
	  case 4:
	    break;
	  }
	break;
      }
    case 2:
      --i;
      break;
    }
}
