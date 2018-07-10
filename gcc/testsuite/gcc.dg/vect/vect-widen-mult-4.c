/* { dg-require-effective-target vect_int } */

#define SIGNEDNESS_1 signed
#define SIGNEDNESS_2 signed

#include "vect-widen-mult-1.c"

/* { dg-final { scan-tree-dump "vect_recog_widen_mult_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_widen_mult_qi_to_hi } } } */
