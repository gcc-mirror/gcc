/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

void bar ();
void foo (int n)
{
  if (n > 0)
    {
      int j = 0;
      do
	{
	  if (n > 0)
	    {
	      int i = 0;
	      do
		{
		  bar ();
		}
	      while (i < n);
	    }
	}
      while (j < n);
    }
}

/* { dg-final { scan-tree-dump-times "if" 1 "fre1" } } */
