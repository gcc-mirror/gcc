/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details" } */

int foo (int i);
int bar2 (int j)
{
  if (j > 2)
    {
      if (j < 7)
	return foo (j + 1);
      else
	return foo (j + 2);
    }
  return j;
}


/* { dg-final { scan-tree-dump "\\\[4, 7\\\]" "evrp" } } */
