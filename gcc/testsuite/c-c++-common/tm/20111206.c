/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O2" } */
/* This test case triggered block sharing between the two transactions.  */

void func1 (void) __attribute__ ((transaction_callable, used));
long func2 (void) __attribute__ ((transaction_callable, used));
unsigned long rand (void);

void client_run (void)
{
  long types[100];
  long i;

  for (i = 0; i < 100; i++)
    {
      long action = rand ();

      switch (action)
	{
	case 0:
	  {
	    __transaction_relaxed
	    {
	      long bill = func2 ();
	      if (bill >= 0)
		{
		  func1 ();
		}
	    }
	    break;
	  }

	case 1:
	  {
	    long n;
	    __transaction_relaxed
	    {
	      for (n = 0; n < 100; n++)
		{
		  long t = types[n];
		  switch (t)
		    {
		    case 0:
		      func1 ();
		      break;
		    }
		}
	    }
	    break;
	  }
	}
    }
}
