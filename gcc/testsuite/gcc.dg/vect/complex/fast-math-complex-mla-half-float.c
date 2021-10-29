/* { dg-do compile } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE _Float16
#define N 200
#include "complex-mla-template.c"
/* { dg-final { scan-tree-dump "Found COMPLEX_FMA_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMA" "vect" } } */
