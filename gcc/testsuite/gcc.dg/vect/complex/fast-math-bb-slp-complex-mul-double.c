/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define TYPE double
#define N 16
#include "complex-mul-template.c"

/* { dg-final { scan-tree-dump "Found COMPLEX_MUL_CONJ" "slp1" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_MUL" slp1" } } */
