/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-optimized" } */

#include <stdarg.h>
#include <stdlib.h>

 #define N 1600
 #define DIFF 2558402

unsigned int ub[N];
unsigned int uc[N];

 /* Reduction of unsigned-int.  */

 void main1 (unsigned int x, unsigned int max_result, unsigned int min_result)
 {
   int i;
   unsigned int udiff = 2;
   unsigned int umax = x;
   unsigned int umin = x;

   /* Summation.  */
   for (i = 0; i < N; i++) {
     udiff += (ub[i] - uc[i]);
   }

   /* Maximum.  */
   for (i = 0; i < N; i++) {
     umax = umax < uc[i] ? uc[i] : umax;
   }

   /* Minimum.  */
   for (i = 0; i < N; i++) {
     umin = umin > uc[i] ? uc[i] : umin;
   }

   /* check results:  */
   if (udiff != DIFF)
     abort ();
   if (umax != max_result)
     abort ();
   if (umin != min_result)
     abort ();
 }

 __attribute__((noinline))
 void init_arrays ()
 {
   int i;

   ub[0] = 1;
   uc[0] = 1;
   for (i=1; i<N; i++)
     {
       ub[i] = i * 3;
       uc[i] = i;
     }
}

int main (void)
{ 
  init_arrays ();
  main1 (2000, 2000, 1);
  main1 (0, 1599, 0);
  return 0;
}


/* { dg-final { scan-tree-dump-times "Detected reduction" 3 "parloops" } } */
/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 4 "parloops" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

