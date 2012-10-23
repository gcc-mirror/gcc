/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-20.i "^\n*A B\n*$" } } */
#define A(x) x
A B
