/* { dg-require-effective-target vect_int } */

#define SIGNEDNESS unsigned

#include "vect-mulhrs-1.c"

/* { dg-final { scan-tree-dump "vect_recog_mulhs_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump {\.MULHS} "vect" { target vect_mulhrs_hi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_mulhrs_hi } } } */
