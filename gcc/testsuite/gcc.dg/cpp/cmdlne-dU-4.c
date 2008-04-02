/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-4.i "^\n*#undef A\n*$" } } */
#if defined(A)
#endif
