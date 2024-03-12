/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details -fdump-tree-phiopt2-details -fdump-tree-optimized" } */

#define SAT(x) (x < 0 ? 0 : (x > 255 ? 255 : x))

unsigned char
foo (unsigned char *p, int i)
{
  if (i < 0)
    return 0;
  {
    int t;
    if (i > 255)
      t = 255;
    else
      t = i;
    return t;
  }
}

/* Because of the way PHIOPT works, it only does the merging of BBs after it is done so we get the case were we can't
   optimize the above until phiopt2 right now.  */
/* { dg-final { scan-tree-dump-times "COND_EXPR .*and PHI .*converted to straightline code" 2 "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "COND_EXPR .*and PHI .*converted to straightline code" 0 "phiopt2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "= MIN_EXPR" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "= MAX_EXPR" 1 "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "= MIN_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= MAX_EXPR" 1 "optimized"  } } */
