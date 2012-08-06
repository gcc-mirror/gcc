/* { dg-do link } */
/* { dg-options "-O2" } */

/* Based on f3 from vrp63.c, but with switch instead of if-chain.  */

extern void link_error (void);

void
f3 (int s)
{
  if (s >> 3 == -2)
    /* s in range [ -16, -9].  */
    ;
  else
    {
      /* s in range ~[-16, -9], so none of the case labels can be taken.  */
      switch (s)
	{
	case -16:
	case -12:
	case -9:
	  link_error ();
	  break;
	default:
	  break;
	}
    }
}

int
main ()
{
  return 0;
}
