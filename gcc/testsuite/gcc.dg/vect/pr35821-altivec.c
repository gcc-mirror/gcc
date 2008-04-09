/* { dg-do compile { target { powerpc_altivec_ok } } } */

#include "altivec.h"

void
foo (float f_gain1, int n_tail, float * __restrict__ f_in_hptr,
     float * __restrict__ f_out_hptr)
{
  int i;
  vector float *v_f_in_hptr, *v_f_out_hptr;

  f_in_hptr = ( float* )v_f_in_hptr;
  f_out_hptr = ( float* )v_f_out_hptr;

  for( i = 0 ; i < n_tail ; i++ ) {
   f_out_hptr[0] = f_in_hptr[0] * f_gain1;
   f_in_hptr++;
   f_out_hptr++;
  }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

