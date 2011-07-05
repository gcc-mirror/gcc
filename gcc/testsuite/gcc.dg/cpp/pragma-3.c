/* 
   { dg-options "-fopenmp" }
   { dg-do preprocess }
   { dg-require-effective-target fopenmp }
 */

void foo (void)
{
  int i1, j1, k1;
#define p parallel
#define P(x) private (x##1)
#define S(x) shared (x##1)
#define F(x) firstprivate (x##1)
#pragma omp \
  p \
  P(i) \
  S(j) \
  F(k)
  ;
}

/* 
   The bug here was that we had a line like:
       # 33554432 "../../gcc/testsuite/gcc.dg/cpp/pragma-3.c"
   
   Before line:

       #pragma omp parallel private (i1) shared (j1) firstprivate (k1)

   Note the very big integer there.  Normally we should just have
   this:
   
       # 13 "../../gcc/testsuite/gcc.dg/cpp/pragma-3.c"
       #pragma omp parallel private (i1) shared (j1) firstprivate (k1)

   So let's check that we have no line with a number of 3 or more
   digit after #:

   { dg-final { scan-file-not pragma-3.i "# \[0-9\]{3} \[^\n\r\]*pragma-3.c" } }
*/
