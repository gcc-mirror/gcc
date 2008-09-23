/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file-not cmdlne-dU-23.i "__FILE__" } } */
#ifdef __FILE__
#endif
