/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int foo (int i, int j)
{
  if (i < j)
    {
      if (i <= j)
	return j > i;
      else
	return 0;
    }
  return 1;
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } }  */
