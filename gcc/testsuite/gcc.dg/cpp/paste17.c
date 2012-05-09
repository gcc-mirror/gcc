 /* { dg-options "-ftrack-macro-expansion=2" } */
/* { dg-do preprocess } */

#define do_paste 1.0e ## -1

do_paste

/* { dg-final {scan-file paste17.i "1.0e- 1" } }*/
