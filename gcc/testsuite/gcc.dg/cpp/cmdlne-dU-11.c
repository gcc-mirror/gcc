/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-11.i "^\n*\n*$" } } */
#define A B
#if 0
A
#endif
