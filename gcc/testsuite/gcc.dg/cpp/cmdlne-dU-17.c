/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-final { scan-file cmdlne-dU-17.i "^\n*1\n+#define __STDC__ 1\n*$" } } */
__STDC__
