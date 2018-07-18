/* { dg-do compile } */

int sy;

void
fo (char o5)
{
  char yh = 0;

  if (o5 == 0)
    return;

  while (o5 != 0)
    if (0)
      {
	while (yh != 0)
	  {
	    o5 = 0;
	    while (o5 < 2)
	      {
		sy &= yh;
		if (sy != 0)
		  {
km:
		    sy = yh;
		  }
	      }
	    ++yh;
	  }
      }
    else
      {
	o5 = sy;
	goto km;
      }
}

void
on (void)
{
  fo (sy);
}
