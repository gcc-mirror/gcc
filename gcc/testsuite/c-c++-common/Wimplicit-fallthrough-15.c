/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Another nested switch.  Check that we don't warn here.  */

void
f (int i)
{
  int j = 0;
  switch (i)
    {
    case 0:
    case 1:
      j = 10;
      __attribute__((fallthrough));
    case 2:
      j += 10;
      break;
    case 3:
      switch (i)
	{
	case 5:
	  j += 2;
	  __attribute__((fallthrough));
	case 6:
	  j += 4;
	  break;
	}
   }
}
