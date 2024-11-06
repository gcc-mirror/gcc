/* N3355 - Named loops.  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

extern void abort (void);

void
foo (int x)
{
  int i, j, k, l, m;
 label1:
  for (i = 0; i < 2; ++i)
    {
      if (i == 1)
	{
	  if (x != 11)
	    abort ();
	  return;
	}
     label2:
      switch (i)
	{
	 label3:
	case 0:
	  for (j = 0; j < 2; ++j)
	    {
	      if (j == 1)
		{
		  if (x != 8)
		    abort ();
		  return;
		}
	     label4:
	      for (k = 0; k < 2; ++k)
		{
		  if (k == 1)
		    {
		      if (x != 6)
			abort ();
		      return;
		    }
		  l = 0;
		 label5:
		  while (l < 2)
		    {
		      if (l == 1)
			{
			  if (x != 4)
			    abort ();
			  return;
			}
		      ++l;
		      m = 0;
		     label6:
		      do
			{
			  if (m == 1)
			    {
			      if (x != 2)
				abort ();
			      return;
			    }
			  ++m;
			 label7:
			  switch (x)
			    {
			    case 0:
			      break label7;
			    case 1:
			      break label6;
			    case 2:
			      continue label6;
			    case 3:
			      break label5;
			    case 4:
			      continue label5;
			    case 5:
			      break label4;
			    case 6:
			      continue label4;
			    case 7:
			      break label3;
			    case 8:
			      continue label3;
			    case 9:
			      break label2;
			    case 10:
			      break label1;
			    case 11:
			      continue label1;
			    default:
			      abort ();
			      break;
			    }
			  if (x)
			    abort ();
			  return;
			}
		      while (m < 2);
		      if (x != 1 || m != 1)
			abort ();
		      return;
		    }
		  if (x != 3 || l != 1 || m != 1)
		    abort ();
		  return;
		}
	      if (x != 5 || k != 0 || l != 1 || m != 1)
		abort ();
	      return;
	    }
	  if (x != 7 || j != 0 || k != 0 || l != 1 || m != 1)
	    abort ();
	  return;
	}
      if (x != 9 || j != 0 || k != 0 || l != 1 || m != 1)
	abort ();
      return;
    }
  if (x != 10 || i != 0 || j != 0 || k != 0 || l != 1 || m != 1)
    abort ();
}

void
bar (int x)
{
  int i, j;
 label1:
  for (i = 0; i < 2; ++i)
    {
      if (i == 1)
	{
	  if (x != 1)
	    abort ();
	  return;
	}
      for (j = 0; j < 2; ++j)
	if (j == 1)
	  abort ();
	else if (x == 0)
	  break label1;
	else if (x == 1)
	  continue label1;
	else
	  abort ();
      abort ();
    }
  if (x != 0)
    abort ();
}

int
main ()
{
  for (int n = 0; n <= 11; ++n)
    foo (n);
  bar (0);
  bar (1);
}
