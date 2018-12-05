/* Rematerialize thru casts if the range fits the LHS type. */
/* { dg-do compile } */
/* { dg-options "-O2  -fdump-tree-rvrp" } */

extern void kill(int i);
extern void keep(int i);

void
foo (int i)
{
  int b = i + 10;
  if (i >= 10 && i <= 100)
    {
      /* i has a range of [10, 100]  */
      char c = (char) b;
      if (c < 30)
	{
	  /* If we wind back thru the cast with the range of c being [10,29]
	   * from the branch, and recognize that the range of b recalcualted 
	   * iuses i which fits within * a cast to c, then we can use the range
	   * for 'c' with 'b' as well and RVRP should be able to kill the call.
	   * */
	  if (b > 29)
	    kill (b);
	}
    }
}

/* { dg-final { scan-tree-dump-times "kill \\(" 0 "rvrp"} } */

