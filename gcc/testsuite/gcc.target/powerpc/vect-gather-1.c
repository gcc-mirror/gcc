/* { dg-do compile } */
/* Profitable from Power8 since it supports efficient unaligned load.  */
/* { dg-options "-Ofast -mdejagnu-cpu=power8 -fdump-tree-vect-details -fdump-tree-forwprop4" } */

#ifndef INDEXTYPE
#define INDEXTYPE unsigned int
#endif
double vmul(INDEXTYPE *rowstart, INDEXTYPE *rowend,
	    double *luval, double *dst)
{
  double res = 0;
  for (const INDEXTYPE * col = rowstart; col != rowend; ++col, ++luval)
        res += *luval * dst[*col];
  return res;
}

/* With gather emulation this should be profitable to vectorize from Power8.  */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* The index vector loads and promotions should be scalar after forwprop.  */
/* { dg-final { scan-tree-dump-not "vec_unpack" "forwprop4" } } */
