/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */

/* Check that the inner if is transformed into CSELs.  */

int
foo (int *x, int *z, int a)
{
  int b = 0;
  int c = 0;
  int d = 0;
  int i;

  for (i = 0; i < a; i++)
    {
      if (x[i] < c)
	{
	  b = z[i];
	  if (c < b)
	    {
	      c = b;
	      d = i;
	    }
	}
    }

  return c + d;
}

/* { dg-final { scan-rtl-dump "if-conversion succeeded through noce_convert_multiple_sets" "ce1" } } */
