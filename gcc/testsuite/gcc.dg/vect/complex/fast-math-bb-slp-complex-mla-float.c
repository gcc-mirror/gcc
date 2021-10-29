/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_float } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE float
#define N 16
#include "complex-mla-template.c"
/* { dg-final { scan-tree-dump "Found COMPLEX_FMA_CONJ" "slp1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMA" "slp1" { xfail *-*-* } } } */
