/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt" } */
double
foo() {
  long n3 = 3450000, xtra = 7270;
  long i,ix;
  long j;
  double Check;

  /* Section 3, Conditional jumps */
  j = 0;
  {
    for (ix=0; ix<xtra; ix++)
      {
        for(i=0; i<n3; i++)
          {
            if(j==1)       j = 2;
            else           j = 3;
            if(j>2)        j = 0;
            else           j = 1;
            if(j<1)        j = 1;
            else           j = 0;
          }
      }
  }
  Check = Check + (double)j;
  return Check;
}

/* the above if statements in loop should be optimized to just `j ^ 1`
   and should not be (type)(j != 1).  */
/* { dg-final { scan-tree-dump-not " != 1" "phiopt2"} } */
/* { dg-final { scan-tree-dump-times " \\^ 1" 1 "phiopt2"} } */

