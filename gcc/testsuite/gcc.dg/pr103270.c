/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

void test(int a, int* i)
{
  for (; a < 5; ++a)
    {
      int b = 0;
      int c = 0;
      for (; b != -11; b--)
	for (int d = 0; d ==0; d++)
	  {
	    *i += c & a;
	    c = b;
	  }
    }
}

/* { dg-final { scan-tree-dump-not "extra loop exit heuristics of edge\[^:\]*:" "profile_estimate"} } */
