/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int foo (int i)
{
  if (i)
    {
      if (i)
	return 0;
      else
	return 1;
    }
  return 0;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
