/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define SH_SUM 210
#define CH_SUM 120

int main1 ()
{
  int i;
  signed short data_sh[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28};
  signed char data_ch[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  signed int intsum = 0;
  signed short shortsum = 0;

  /* widenning sum: sum shorts into int.  */
  for (i = 0; i < N; i++){
    intsum += data_sh[i];
  }

  /* check results:  */
  if (intsum != SH_SUM)
    abort ();

  /* widenning sum: sum chars into int.  */
  intsum = 0;
  for (i = 0; i < N; i++){
    intsum += data_ch[i];
  }

  /* check results:  */
  if (intsum != CH_SUM)
    abort ();

  /* widenning sum: sum chars into short.
     The widening-summation pattern is currently not detected because of this
     patch:

     2005-12-26  Kazu Hirata  <kazu@codesourcery.com>
                                                                                
        PR tree-optimization/25125
   */
  for (i = 0; i < N; i++){
    shortsum += data_ch[i];
  }

  /* check results:  */
  if (shortsum != CH_SUM)
    abort ();

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vect_recog_widen_sum_pattern: detected" 3 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_sum_pattern: detected" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target vect_widen_sum } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
