/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sccp-details" } */

int
foo (unsigned int n)
{
  int i, r = 1;
  if (n > 0)
    {
      asm ("");
      if (n < 10)
	{
	  asm ("");
	  do
	    {
	      --n;
	      r *= 2;
	    }
	  while (n > 0);
	}
    }
  return r + n;
}

/* { dg-final { scan-tree-dump "# of iterations \[^\n\r]*, bounded by 8" "sccp" } } */
/* { dg-final { cleanup-tree-dump "sccp" } } */
