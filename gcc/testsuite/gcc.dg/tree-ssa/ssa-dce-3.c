/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce" } */

int main(void)
{
  unsigned i, j;

  for (i = 1, j = 0; i != 0; i+=2)
    {
      j += 500;
      if (j % 7)
	{
	  j++;
	}
      else
	{
	  j--;
	}
    }

  return 0;
}

/* We should eliminate the inner condition, but the loop must be preserved
   as it is infinite.  Therefore there should be just one phi node (for i):  */
/* { dg-final { scan-tree-dump-times "PHI " 1 "cddce"} } */

/* And one if (for the exit condition of the loop):  */
/* { dg-final { scan-tree-dump-times "if " 1 "cddce"} } */

/* { dg-final { cleanup-tree-dump "cddce" } } */
