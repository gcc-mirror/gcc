/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize -fdump-tree-ifcvt-stats" { target *-*-* } } */

#include <stdarg.h>
#include <signal.h>

#define N 16
#define MAX 42

extern void abort(void); 

int main1 ()
{  
  int A[N] = {36,39,42,45,43,32,21,12,23,34,45,56,67,78,89,11};

  int i, j;

  for (i = 0; i < N; i++)
    {
      j = A[i];
      A[i] = ( j >= MAX ? MAX : 0); 
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (A[i] > MAX)
	abort ();
    }

  return 0;
}



/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
