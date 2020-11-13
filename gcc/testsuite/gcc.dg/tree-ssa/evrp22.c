/* See backwards thru casts if the range fits the LHS type. */
/* { dg-do compile } */
/* { dg-options "-O2  -fdump-tree-evrp" } */

extern void kill(int i);
extern void keep(int i);

void
foo (int i)
{
  if (i >= 10)
    {
      if (i <= 100)
	{
	  /* i has a range of [10, 100]  */
	  char c = (char) i;
	  if (c < 30)
	    {
	      /* If we wind back thru the cast with the range of c being [10,29]
	       * from the branch, and recognize that the range of i fits within
	       * a cast to c, then there is no missing information in a cast
	       * back to int. We can use the range calculated for 'c' with 'i'
	       * as well and Ranger should be able to kill the call.  */
	      if (i > 29)
		kill (i);
	    }
	}
      /* i has a range of [10, MAX]  */
      char d  = (char) i;
      if (d < 30)
	{
	  /* Here, a cast to a char and back is NOT equivalent, so we cannot use
	   * the value of d to remove the call.  */
	  if (i > 29)
	    keep (i);
	}

    }
}

/* { dg-final { scan-tree-dump-times "kill \\(" 0 "evrp"} } */
/* { dg-final { scan-tree-dump-times "keep \\(" 1 "evrp"} } */

