/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fno-tree-fre -fdump-tree-vrp1 -fdisable-tree-ethread" } */

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

/* { dg-final { scan-tree-dump-not "PHI" "vrp1" } } */
/* { dg-final { scan-tree-dump-not "goto" "vrp1" } } */
/* { dg-final { scan-tree-dump-not "if" "vrp1" } } */
