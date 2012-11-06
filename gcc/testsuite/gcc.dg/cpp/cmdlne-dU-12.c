/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-12.i "^\n*#define A 1\n*$" } } */
#define A 1
#if A
#endif
