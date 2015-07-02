/* Check calling convention in the vector ABI for single element vectors.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "vlr\t%v24,%v26" 1 } } */

typedef int  __attribute__((vector_size(16))) v4si;

typedef __int128_t __attribute__((vector_size(16))) v1ti;

v1ti foo (v4si a, v1ti b) { return b; }
