/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define TYPE double
#define N 200
#include "complex-mul-template.c"
/* { dg-final { scan-tree-dump "Found COMPLEX_MUL_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_MUL" "vect" } } */
