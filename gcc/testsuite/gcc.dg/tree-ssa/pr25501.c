/* PR tree-optimization/25501
   The PHI merge pass used to try to merge PHI nodes that cannot
   actually merged, causing a segfault later.  Make sure that does not
   happen any more.  */

/* { dg-options "-O1 -fdump-tree-mergephi1" } */

int
foo (int a)
{
  int b;
  int c;
  int d;

  if (a == 2)
    b = 3;
  else
    b = 5;

  c = 7;

  d = 11;

  for (;;)
    {
      if (d == 5)
	break;

      d = b;
    }

  return 13;
}

/* { dg-final { scan-tree-dump-times "Removing basic block" 0 "mergephi1"} } */
