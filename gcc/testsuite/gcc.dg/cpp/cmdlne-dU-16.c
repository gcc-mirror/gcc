/* { dg-do preprocess } */
/* { dg-options "-P -dU" } */
/* { dg-options "-P -dU -std=c89" { target *-*-solaris2.* } } */
/* { dg-final { scan-file cmdlne-dU-16.i "^\n*#define __STDC__ 1\n*$" } } */
#ifdef __STDC__
#endif
