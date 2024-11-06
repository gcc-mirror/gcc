/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_half } */
/* { dg-require-effective-target float16 } */
/* { dg-additional-options "-ffast-math -fno-tree-loop-vectorize" } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */

#define TYPE _Float16
#define N 16
#include "complex-mls-template.c"

/* { dg-final { scan-tree-dump "Found COMPLEX_FMS_CONJ" "slp1"  { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_FMS" "slp1"  { xfail *-*-* } } } */
