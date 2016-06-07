/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

int bar(int);

void foo (int bound)
{
  int i, ret = 0;
  for (i = 0; i <= bound; i++)
    {
      if (i < bound - 2)
	global += bar (i);
      /* The following test is redundant with the loop bound check in the
         for stmt and thus eliminated by FRE which makes the controlled
	 stmt always executed and thus equivalent to 100%.  Thus the
	 heuristic only applies three times.  */
      if (i <= bound)
	global += bar (i);
      if (i + 1 < bound)
	global += bar (i);
      if (i != bound)
	global += bar (i);
    }
}

/* { dg-final { scan-tree-dump-times "loop iv compare heuristics: 98.0%" 3 "profile_estimate"} } */
