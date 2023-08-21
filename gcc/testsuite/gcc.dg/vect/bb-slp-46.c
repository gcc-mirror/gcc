/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized" } */

int a[4], b[4];
int foo ()
{
  int tem0 = a[0] + b[0];
  int temx = tem0 * 17;  /* this fails without a real need */
  int tem1 = a[1] + b[1];
  int tem2 = a[2] + b[2];
  int tem3 = a[3] + b[3];
  int temy = tem3 * 13;
  a[0] = tem0;
  a[1] = tem1;
  a[2] = tem2;
  a[3] = tem3;
  return temx / temy;
}

/* We should extract the live lane from the vectorized add rather than
   keeping the original scalar add.
   ???  Because of a too conservative check we fail for temx here.  */
/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
/* { dg-final { scan-tree-dump "extracting lane for live stmt" "slp2" } } */
/* { dg-final { scan-tree-dump-times "extracting lane for live stmt" 2 "slp2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "tem3_\[0-9\]\+ = " "optimized" } } */
/* { dg-final { scan-tree-dump-not "tem0_\[0-9\]\+ = " "optimized" { xfail *-*-* } } } */
