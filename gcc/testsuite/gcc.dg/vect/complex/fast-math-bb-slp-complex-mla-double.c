/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define TYPE double
#define N 16
#include "complex-mla-template.c"

/* { dg-final { scan-tree-dump "Found COMPLEX_FMA_CONJ" "slp1" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMA" "slp1" } } */
