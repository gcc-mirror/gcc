/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

void bar (void);
int foo (int i, int j)
{
  int res = 1;
  if (i < j)
    {
      /* We should be able to simplify the following conditional
         during propagation.  */
      if (i > j)
	res = 0;
    }
  /* And compute res as having a value-range of [1,1].  */
  if (res)
    return i;
  return j;
}

/* { dg-final { scan-tree-dump "res_.: \\\[1, 1\\\]" "vrp1" } } */
/* { dg-final { scan-tree-dump-not "Threaded" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
