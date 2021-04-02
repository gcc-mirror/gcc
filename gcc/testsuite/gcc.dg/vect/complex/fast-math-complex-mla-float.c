/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_float } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE float
#define N 200
#include "complex-mla-template.c"
