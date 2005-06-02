/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

foo (int k, int j)
{
  if (k >= 10)
    {
      if (j > k)
	{
	  /* We should fold this to if (0).  */
	  if (j < 10)
	    abort ();
	}
    }

  return j;
}

main()
{
  foo (10, 3);
  return 0;
}
