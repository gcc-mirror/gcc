/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-ffast-math -fno-tree-loop-vectorize" } */

#define UNROLL

#define TYPE float
#define N 16
#include "complex-add-template.c"

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT90" 16 "slp1" { target { vect_complex_add_float } } } } */
/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT270" 16 "slp1" { target { vect_complex_add_float } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "slp1" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "slp1" } } */
