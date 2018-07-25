/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#ifndef SIGNEDNESS
#define SIGNEDNESS unsigned
#define BASE_B 4
#define BASE_C 40
#endif

#include "vect-over-widen-15.c"

/* { dg-final { scan-tree-dump {Splitting statement} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \+} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 1} "vect" } } */
/* { dg-final { scan-tree-dump-not {vect_recog_cast_forwprop_pattern: detected} "vect" } } */
/* { dg-final { scan-tree-dump {vector[^ ]* int} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
