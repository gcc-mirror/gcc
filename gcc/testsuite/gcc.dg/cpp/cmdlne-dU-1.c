/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-1.i "^\n*#undef A\n*$" } } */
#ifdef A
#endif
