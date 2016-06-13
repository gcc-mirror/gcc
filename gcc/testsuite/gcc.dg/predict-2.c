/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

int bar(int);

void foo (int base, int bound)
{
  int i, ret = 0;
  for (i = base; i < bound; i++)
    {
      if (i > bound * bound)
	global += bar (i);
      if (i > bound + 10)
	global += bar (i);
      if (i <= bound + 10)
	global += bar (i);
      if (i > base + 10)
	global += bar (i);
      if (i < base - 10)
	global += bar (i);
    }
}

/* { dg-final { scan-tree-dump-not "guess loop iv compare heuristics of edge\[^:\]*:" "profile_estimate"} } */
