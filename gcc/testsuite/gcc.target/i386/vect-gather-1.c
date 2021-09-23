/* { dg-do compile } */
/* { dg-options "-Ofast -msse2 -fdump-tree-vect-details -fdump-tree-forwprop4" } */

#ifndef INDEXTYPE
#define INDEXTYPE int
#endif
double vmul(INDEXTYPE *rowstart, INDEXTYPE *rowend,
	    double *luval, double *dst)
{
  double res = 0;
  for (const INDEXTYPE * col = rowstart; col != rowend; ++col, ++luval)
        res += *luval * dst[*col];
  return res;
}

/* With gather emulation this should be profitable to vectorize
   even with plain SSE2.  */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* The index vector loads and promotions should be scalar after forwprop.  */
/* { dg-final { scan-tree-dump-not "vec_unpack" "forwprop4" } } */
