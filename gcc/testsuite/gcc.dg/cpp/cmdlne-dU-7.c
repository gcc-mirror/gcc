/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-7.i "^\n*B\n+#define A B\n+C\n+#define A C\n*$" } } */
#define A B
A
#undef A
#define A C
A
