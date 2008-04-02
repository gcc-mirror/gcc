/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-19.i "^\n*B\n+#define A B\n+#undef A\n*$" } } */
#define A B
A
#undef A
#ifdef A
#endif
