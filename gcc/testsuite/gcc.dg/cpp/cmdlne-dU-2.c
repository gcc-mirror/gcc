/* { dg-do preprocess } */
/* { dg-options "-nostdinc -P -dU" } */
/* { dg-final { scan-file cmdlne-dU-2.i "^\n*#define A *\n*$" } } */
#define A
#ifdef A
#endif
