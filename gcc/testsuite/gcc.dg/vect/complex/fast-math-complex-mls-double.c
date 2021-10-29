/* { dg-do compile } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE double
#define N 200
#include "complex-mls-template.c"
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMA" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMS_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMS" "vect" } } */
