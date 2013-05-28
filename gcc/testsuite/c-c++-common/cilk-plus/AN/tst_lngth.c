/* { dg-do compile } */
/* { dg-options " -fcilkplus " } */

#if HAVE_IO
#include <stdio.h>
#endif

#define N 256
int A[N], B[N];

int
main ()
{
    A[0:(N / 4)] = A[4]+ B[0:(N / 2):2]; /* { dg-error "length mismatch between" } */ 
    A[0:(N / 4)] = B[0:(N / 2):2] + N; /* { dg-error "length mismatch between" } */
    A[0:(N / 4)] = B[0:(N / 2):2] + A[4]; /* { dg-error "length mismatch between" } */
    return 0;
}

