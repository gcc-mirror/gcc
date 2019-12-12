/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

int bar(int);

void foo (int bound)
{
  int i, ret = 0;
  for (i = 0; i < 10; i++)
    {
      if (i < 5)
	global += bar (i);
    }
}

/* { dg-final { scan-tree-dump "  loop iv compare heuristics of edge\[^:\]*: 50.00%" "profile_estimate"} } */
