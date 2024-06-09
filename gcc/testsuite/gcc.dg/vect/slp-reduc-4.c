/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

unsigned int uc[N];

/* Vectorization of reduction using loop-aware SLP.  */

__attribute__ ((noinline))
int main1 (int n, int res0, int res1, int res2, int res3, int res4, int res5, int res6, int res7)
{
  int i;
  unsigned int max0 = 5, max1 = 10, max2 = 20, max3 = 30, max4 = 2, max5 = 13, max6 = 7, max7 = 313;

  for (i = 0; i < n; i++) {
    max2 = max2 < uc[8*i+2] ? uc[8*i+2] : max2;
    max3 = max3 < uc[8*i+3] ? uc[8*i+3] : max3;
    max1 = max1 < uc[8*i+1] ? uc[8*i+1] : max1;
    max7 = max7 < uc[8*i+7] ? uc[8*i+7] : max7;
    max6 = max6 < uc[8*i+6] ? uc[8*i+6] : max6;
    max0 = max0 < uc[8*i] ? uc[8*i] : max0;
    max4 = max4 < uc[8*i+4] ? uc[8*i+4] : max4;
    max5 = max5 < uc[8*i+5] ? uc[8*i+5] : max5;
  }

  /* Check results:  */
  if (max0 != res0
      || max1 != res1
      || max2 != res2
      || max3 != res3
      || max4 != res4
      || max5 != res5
      || max6 != res6
      || max7 != res7)
    abort ();

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      uc[i] = i+3;
      __asm__ volatile ("");
    }

  main1 (N/8, 123, 124, 125, 126, 127, 128, 129, 313);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_min_max } } } */
/* For variable-length SVE, the number of scalar statements in the
   reduction exceeds the number of elements in a 128-bit granule.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { ! vect_multiple_sizes } xfail { vect_no_int_min_max || { aarch64_sve && vect_variable_length } } } } } */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_multiple_sizes && { ! { vect_load_lanes && vect_strided8 } } } } } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" { xfail { aarch64_sve && vect_variable_length } } } } */

