/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-16.i "^\n*#define __STDC__ 1\n*$" } } */
#ifdef __STDC__
#endif
