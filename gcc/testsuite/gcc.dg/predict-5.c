/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

int bar (int);

void foo (int base, int bound)
{
  int i, ret = 0;
  for (i = base; i <= bound; i++)
    {
      if (i > base)
	global += bar (i);
      if (i > base + 1)
	global += bar (i);
      if (i >= base + 3)
	global += bar (i);
      if (i - 2 >= base)
	global += bar (i);
    }
}

/* { dg-final { scan-tree-dump-times "guess loop iv compare heuristics of edge\[^:\]*: 98.0%" 4 "profile_estimate"} } */
