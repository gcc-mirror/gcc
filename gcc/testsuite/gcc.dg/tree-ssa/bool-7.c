/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(_Bool x)
{
  int y;
  if (x != 1)
    y = 0;
  else
    y = 1;
  return y;
}

/* There should be no != 1. Though PHI-OPT or invert_truth does not
   fold its tree.  */
/* { dg-final { scan-tree-dump-times "!= 1" 0 "optimized" { xfail *-*-* } }  }*/

/* { dg-final { cleanup-tree-dump "optimized" } } */
