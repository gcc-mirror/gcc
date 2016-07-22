/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread1-details -fdump-tree-dce2" } */

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

/* There are 4 FSM jump threading opportunities, all of which will be
   realized, which will eliminate testing of FLAG, completely.  */
/* { dg-final { scan-tree-dump-times "Registering FSM" 4 "thread1"} } */

/* There should be no assignments or references to FLAG, verify they're
   eliminated as early as possible.  */
/* { dg-final { scan-tree-dump-not "if .flag" "dce2"} } */
