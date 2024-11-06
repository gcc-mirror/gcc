/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target vect_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define TYPE double
#define N 200
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" { target { vect_complex_add_double } } } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" { target { vect_complex_add_double } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "vect" } } */
