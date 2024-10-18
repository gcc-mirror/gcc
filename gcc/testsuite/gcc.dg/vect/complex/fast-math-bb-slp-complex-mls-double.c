/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-ffast-math -fdump-tree-vect-details" } */

#define TYPE double
#define N 16
#include "complex-mls-template.c"

/* { dg-final { scan-tree-dump "Found COMPLEX_FMS_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMS" "vect" } } */
