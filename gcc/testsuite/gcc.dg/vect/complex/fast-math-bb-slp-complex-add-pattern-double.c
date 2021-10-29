/* { dg-do compile } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE double
#define N 16
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "slp1" { target { vect_complex_add_double } } } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "slp1" { target { vect_complex_add_double } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "slp1" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "slp1" } } */
