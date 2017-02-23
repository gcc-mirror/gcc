/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize -fdump-tree-ifcvt-stats-blocks-details" { target *-*-* } } */

#include <stdarg.h>

#define N 16
#define MAX 42

int A[N] = {36,39,42,45,43,32,21,12,23,34,45,56,67,78,89,11};

extern void abort(void); 

int main1 ()
{  
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

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming frequencies" 1 "ifcvt" } } */
