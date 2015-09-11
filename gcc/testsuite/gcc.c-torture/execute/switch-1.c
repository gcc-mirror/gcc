/* Copyright (C) 2003  Free Software Foundation.

   Test that switch statements suitable using case bit tests are
   implemented correctly.

   Written by Roger Sayle, 01/25/2001.  */

extern void abort (void);

int
foo (int x)
{
  switch (x)
    {
    case 4:
    case 6:
    case 9:
    case 11:
      return 30;
    }
  return 31;
}

int
main ()
{
  int i, r;

  for (i=-1; i<66; i++)
    {
      r = foo (i);
      if (i == 4)
	{
	  if (r != 30)
	    abort ();
	}
      else if (i == 6)
	{
	  if (r != 30)
	    abort ();
	}
      else if (i == 9)
	{
	  if (r != 30)
	    abort ();
	}
      else if (i == 11)
	{
	  if (r != 30)
	    abort ();
	}
      else if (r != 31)
	abort ();
    }
  return 0;
}

