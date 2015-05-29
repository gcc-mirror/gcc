/* { dg-do compile } */
/* { dg-options "-O1 -funswitch-loops -fdump-tree-unswitch-blocks" } */

int bla(int p)
{
  unsigned i, s = 1;

  for (i = 4; i < 100; i++)
    {
      if (p)
	s += i/2;
      else
	s *= i/2;
    }

  return s;
}

/* We used to make the probability that the first of the loops created
   by unswitching is entered 100%, which is not correct.  */

/* { dg-final { scan-tree-dump-not "Invalid sum" "unswitch"} } */
/* { dg-final { scan-tree-dump-not "SUCC: 3 .100.0%" "unswitch"} } */
