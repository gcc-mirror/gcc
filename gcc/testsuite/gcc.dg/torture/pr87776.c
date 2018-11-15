/* { dg-do compile } */
/* { dg-additional-options "--param rpo-vn-max-loop-depth=2" } */

int wp;

void
ff (void)
{
  int *s3 = &wp;

  for (wp = 0; wp < 1; ++wp)
    {
      if (wp != 0)
	{
o8:
	  ;
	}
      else
	{
b7:
	  while (wp < 1)
	    {
	    }
	}

      for (*s3 = 0; *s3 < 1; ++*s3)
	{
	  for (wp = 0; wp < 1; ++wp)
	    for (wp = 0; wp < 1; ++wp)
	      {
	      }

	  for (wp = 0; wp < 1; ++wp)
	    goto o8;

	  for (wp = 0; wp < 1; ++wp)
	    goto b7;
	}
    }
}
