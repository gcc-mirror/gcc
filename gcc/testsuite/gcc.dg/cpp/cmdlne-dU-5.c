/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-5.i "^\n*#undef A\n*$" } } */
#ifdef A
#ifdef B
#endif
#endif
