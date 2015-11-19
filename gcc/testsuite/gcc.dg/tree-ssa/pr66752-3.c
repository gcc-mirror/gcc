/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details -fdump-tree-optimized" } */

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

/* There are 3 FSM jump threading opportunities, one of which will
   get cancelled.  */
/* { dg-final { scan-tree-dump-times "Registering FSM" 3 "vrp1"} } */
/* { dg-final { scan-tree-dump-times "Cancelling FSM" 1 "vrp1"} } */

/* There should be no assignments or references to FLAG.  */
/* { dg-final { scan-tree-dump-not "flag" "optimized"} } */
