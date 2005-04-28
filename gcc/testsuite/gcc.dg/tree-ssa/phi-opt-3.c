/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(int a, int b)
{
  if (a == 0)
   return 0;
  if (a != b)
   return a;
 return a;
}

/* There should be no ifs as the PHI arguments, we did not
   optimize this before because PHI-OPT did not look at 
   PHIs which have more than two arguments.  */
/* { dg-final { scan-tree-dump-times "if" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
