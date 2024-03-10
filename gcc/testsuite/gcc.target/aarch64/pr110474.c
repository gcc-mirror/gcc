/* { dg-do compile } */
/* { dg-options "-O3 -mtune=neoverse-n2 -mcpu=neoverse-n1 -fdump-tree-vect-details --param aarch64-vect-unroll-limit=2" } */
/* { dg-final { scan-tree-dump "Choosing vector mode V8HI"  "vect" } } */
/* { dg-final { scan-tree-dump "Choosing epilogue vector mode V8QI"  "vect" } } */

/* Do not increase the the vector factor of the epilog vectorized loop
   for a loop with suggested_unroll_factor > 1.

   before (suggested_unroll_factor=1):
     if N >= 16:
         main vect loop
     if N >= 8:
         epilog vect loop
     scalar code

   before (suggested_unroll_factor=2):
     if N >= 32:
         main vect loop
     if N >= 16:  // May fail to execute vectorized code (e.g. N is 8)
         epilog vect loop
     scalar code

   after  (suggested_unroll_factor=2):
     if N >= 32:
         main vect loop
     if N >= 8:  // The same VF as suggested_unroll_factor=1
         epilog vect loop
     scalar code  */

int
foo (short *A, char *B, int N)
{
  int sum = 0;
  for (int i = 0; i < N; ++i)
    sum += A[i] * B[i];
  return sum;
}
