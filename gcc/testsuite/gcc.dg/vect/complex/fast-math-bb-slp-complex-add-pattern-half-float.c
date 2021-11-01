/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_half } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */
/* { dg-additional-options "-fno-tree-loop-vectorize -funroll-loops" } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE _Float16
#define N 16
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "slp1" { target { vect_complex_add_half } } } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "slp1" { target { vect_complex_add_half } && ! target { arm*-*-* } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "slp1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "slp1" } } */
