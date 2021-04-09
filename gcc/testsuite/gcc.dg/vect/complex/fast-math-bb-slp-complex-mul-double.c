/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE double
#define N 16
#include "complex-mul-template.c"

