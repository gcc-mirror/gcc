/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-9.i "^\n*C\n+#define B C\n+#define A B\n*$" } } */
#define A B
#define B C
A
