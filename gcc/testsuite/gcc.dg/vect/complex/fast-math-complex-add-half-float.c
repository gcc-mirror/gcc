/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target vect_complex_add_half } */
/* { dg-require-effective-target float16 } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */

#define TYPE _Float16
#define N 200
#include "complex-add-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */
