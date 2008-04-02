/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-8.i "^\n*B D\n+#define A\\(x\\) B x\n+#define C D\n*$" } } */
#define A(x) B x
#define C D
A(C)
