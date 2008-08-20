/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-cddce2" } */

int test(int v)
{
  int x = 0;
  int u;
  for (u=0;u<2;u++)
  {
    if (u>v)
    {
      if (u%2==1)
        x++;
    }
  }  
  return x;
}

/* This should be optimized to

    if (v <= 0) goto <L1>; else goto <L3>;

   <L1>:;

    # x_1 = PHI <0(3), 1(1)>;
   <L3>:;
    return x_1;

   retaining only a single conditional.  This doesn't work as nobody
   combines the two tests

    if (v < 0) goto <bb 4>; else goto <bb 3>;

   <bb 3>:

    if (v <= 0) goto <bb 4>; else goto <bb 5>;

   this late in the game.  tree-ssa-ifcombine.c would do it if we would
   unroll the loop during early loop unrolling though.

   For now vrp2 does all the needed folding and threading and cddce2
   provides a nice IL to scan.  */

/* { dg-final { scan-tree-dump-times "if " 1 "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "if " 2 "cddce2" } } */
/* { dg-final { scan-tree-dump "x_. = PHI <0\\\(.\\\), 1\\\(.\\\)>" "cddce2" } } */
/* { dg-final { cleanup-tree-dump "cddce2" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
