/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(_Bool x)
{
  int y;
  if (!x)
    y = 0;
  else
    y = 1;
  return y;
}

/* There should be no == 0.  Though PHI-OPT or invert_truth does not
   fold its tree forwprop is able to clean up the mess.  */
/* { dg-final { scan-tree-dump-times "== 0" 0 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
