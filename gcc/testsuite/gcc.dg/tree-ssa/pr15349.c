/* PR 15349.  Merge two PHI nodes.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-mergephi" } */

int
foo (int a, int b)
{
  int t;

  if (b)
    {
      if (a)
	t = 3;
      else
	t = 5;

      a = 0;
    }
  else
    t = 7;

  return t;
}

/* { dg-final { scan-tree-dump-times "PHI" 1 "mergephi"} } */
