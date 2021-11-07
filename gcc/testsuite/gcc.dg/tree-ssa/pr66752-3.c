/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-threadfull1-details -fdump-tree-thread2" } */

extern int status, pt;
extern int count;
void
foo (int N, int c, int b, int *a)
{
  int i, flag;
  i = b -1;
  flag = 1;
  if (status && i < N && a[i] == b) {
    N--;
    flag = 0;
   if (pt)
     count++;
  }
  else    
    for (i = -1, flag = 1; ++i < N && flag;)
      if (a[i] == b)
        {
          --N;
          flag = 0;
          if (i < N)
            a[i] = a[N];
           else
            a[i] = 0;
          if (pt)
            count++;
        }
 if(status && flag)
   pt--;
}

/* There are 2 jump threading opportunities (which don't cross loops),
   all of which will be realized, which will eliminate testing of
   FLAG, completely.  */
/* { dg-final { scan-tree-dump-times "Registering jump" 2 "threadfull1"} } */

/* We used to remove references to FLAG by DCE2, but this was
   depending on early threaders threading through loop boundaries
   (which we shouldn't do).  However, the late threading passes, which
   run after loop optimizations , can successfully eliminate the
   references to FLAG.  Verify that ther are no references by the late
   threading passes.  */
/* { dg-final { scan-tree-dump-not "if .flag" "thread2"} } */
