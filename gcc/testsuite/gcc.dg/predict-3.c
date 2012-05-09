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
      if (i <= bound)
	global += bar (i);
      if (i + 1 < bound)
	global += bar (i);
      if (i != bound)
	global += bar (i);
    }
}

/* { dg-final { scan-tree-dump-times "loop iv compare heuristics: 100.0%" 4 "profile_estimate"} } */
/* { dg-final { cleanup-tree-dump "profile_estimate" } } */
