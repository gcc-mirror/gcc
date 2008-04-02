/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-18.i "^\n*x 1 y\n+#define A 1\n*$" } } */
#define A 1
x A y
