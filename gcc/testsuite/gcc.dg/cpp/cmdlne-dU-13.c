/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-13.i "^\n*#undef A\n*$" } } */
#ifdef A
#endif
#ifdef A
#endif
