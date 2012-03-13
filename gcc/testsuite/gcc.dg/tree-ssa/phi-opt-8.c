/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized -fdump-tree-phiopt1" } */

int g(int,int);
int f(int t, int c)
{
  int d = 0;
  int e = 0;
  if (t)
    {
      d = 1;
      e = t;
    }
  else d = 0, e = 0;
  return g(e,d);
}

/* This testcase should be reduced to e = t; d = t != 0; in phiopt1
   but currently is not as PHI-OPT does not reduce the t PHI as we have
   two phis.  Note this is fixed with
   http://gcc.gnu.org/ml/gcc-patches/2012-01/msg01195.html .  */
/* { dg-final { scan-tree-dump-not "if" "phiopt1" } } */
/* { dg-final { scan-tree-dump "g .t_\[0-9\]*.D.," "optimized" } } */
/* { dg-final { scan-tree-dump-not "PHI" "optimized" } } */
/* { dg-final { cleanup-tree-dump "phiopt1" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
