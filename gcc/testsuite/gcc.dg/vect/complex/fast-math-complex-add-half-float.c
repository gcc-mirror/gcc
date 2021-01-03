/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_half } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE _Float16
#define N 200
#include "complex-add-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 2 "vect" } } */