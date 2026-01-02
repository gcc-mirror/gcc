/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_half } */
/* { dg-require-effective-target float16 } */
/* { dg-add-options arm_v8_3a_fp16_complex_neon } */
/* { dg-additional-options "-ffast-math -fno-tree-loop-vectorize" } */

#define UNROLL

#define TYPE _Float16
#define N 16
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT90" 5 "slp1" { target { vect_complex_add_half } } } } */
/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT270" 2 "slp1" { target { vect_complex_add_half } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "slp1" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "slp1" } } */
